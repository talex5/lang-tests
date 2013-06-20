module Bindings where

import Text.XML.Light
import Data.Map (Map, insert, lookup, member)
import Data.Maybe (fromMaybe)
import System.FilePath
import Control.Monad (mplus, foldM)
import Text.Regex.Posix
import System.Posix.Files (fileExist, createSymbolicLink, setFileMode)
import Text.JSON (encode)

import Support
import Config
import Selections
import Command (buildCommand)
import Basedir (save, cache)

data WhichEnd = Prepend | Append deriving Show

data AddMode = AddMode { pos :: WhichEnd
		       , defaultValue :: Maybe String
		       , separator :: String
		       } deriving Show

data Mode = Add AddMode | Replace deriving Show

data EnvSource = InsertPath FilePath | Value String deriving Show

data ExecBindingType = InPath | InVar deriving Show

data Binding = EnvironmentBinding VarName Mode EnvSource
	     | ExecutableBinding ExecBindingType String String
	     deriving Show

processEnvironmentBinding :: Element -> Binding
processEnvironmentBinding bindingElem = EnvironmentBinding name mode bindingSource
	where attr attrName = findAttr (mkQName attrName) bindingElem
	      Just name = attr "name"
	      mode = case fromMaybe "prepend" (attr "mode") of
	      	"prepend" -> Add $ AddMode Prepend (attr "default") (fromMaybe [searchPathSeparator] $ attr "separator")
	      	"append" -> Add $ AddMode Append (attr "default") (fromMaybe [searchPathSeparator] $ attr "separator")
		"replace" -> Replace
		m -> error $ "Bad mode " ++ m
	      bindingSource = case (attr "insert", attr "value") of
				  (Nothing, Nothing) -> error "Missing 'insert' or 'value'"
				  (Just i, Nothing) -> InsertPath i
				  (Nothing, Just v) -> Value v
				  (Just _, Just _) -> error "Can't use 'insert' and 'value' together"

processExecutableBinding :: ExecBindingType -> Element -> Binding
processExecutableBinding bindingType bindingElem = ExecutableBinding bindingType name commandName
	where name = requireAttr "name" bindingElem
	      commandName = fromMaybe "run" (findAttr (mkQName "command") bindingElem)

processBinding :: String -> Element -> [Binding]
processBinding "environment" e = [processEnvironmentBinding e]
processBinding "executable-in-path" e = [processExecutableBinding InPath e]
processBinding "executable-in-var" e = [processExecutableBinding InVar e]
processBinding _ _ = []

-- Find all the bindings, in document order
-- Excludes bindings to unselected optional components
collectBindings :: Selections -> [(InterfaceURI, Binding)]
collectBindings sels = do sel <- filterChildrenName (hasZName "selection") (root sels)
                          let iface = requireAttr "interface" sel
                          getBindings True True iface sel
    where getBindings deps commands iface parent =
            do (name, child) <- ziChildren parent
               case name of
                    "command" | commands -> getBindings True False iface child
                    x | (x == "requires" || x == "runner") && deps ->
                             let depIface = requireAttr "interface" child in
                             if depIface `member` (selections sels)
                               then getBindings False False depIface child
                               else []  -- Optional dependency which was not selected
                    _ -> do binding <- processBinding name child
                            [(iface, binding)]

join :: WhichEnd -> String -> Maybe String -> String -> String
join _ _ Nothing new = new
join Prepend sep (Just old) new = new ++ sep ++ old
join Append sep (Just old) new = old ++ sep ++ new

standardDefault :: VarName -> Maybe String
standardDefault "PATH" = Just "/bin:/usr/bin"
standardDefault "XDG_CONFIG_DIRS" = Just "/etc/xdg"
standardDefault "XDG_DATA_DIRS" = Just "/usr/local/share:/usr/share"
standardDefault _ = Nothing

doEnvBinding :: Maybe FilePath -> Env -> Binding -> Env
doEnvBinding mPath env (EnvironmentBinding name mode bindingSource) =
    case (mPath, bindingSource) of
	    (_, Value v) -> use v
	    (Nothing, InsertPath _) -> env
	    (Just implPath, InsertPath i) -> use $ implPath </> i
    where use newValue = insert name (add newValue) env
          add newValue = case mode of
	    Replace -> newValue
	    Add AddMode {pos = whichEnd, defaultValue = def, separator = sep} ->
		join whichEnd sep oldValue newValue
		where oldValue = (Data.Map.lookup name env) `mplus`
				 def `mplus` (standardDefault name)
doEnvBinding _ env _ = env

validateName :: String -> String
validateName name = if name =~ "^[^./'][^/']*$" then name
		    else error $ "Invalid command name: " ++ name

checkRunenv :: Config -> IO ()
checkRunenv config = do execDir <- save ("0install.net" </> "injector") (cache $ basedirs config)
			let execPath = execDir </> "runenv.haskell"
			x <- fileExist execPath
			if x then return ()
			else createSymbolicLink ((resourceDir config) </> "Runenv") execPath

ensureLauncher :: Config -> String -> IO FilePath
ensureLauncher config name = do execDir <- save ("0install.net" </> "injector" </> "executables" </> name) (cache $ basedirs config)
				let execPath = execDir </> name
				x <- fileExist execPath
				if x then return execDir
				else do createSymbolicLink "../../runenv.haskell" execPath
				        setFileMode execDir 0o500
					return execDir

doExecBindings :: Config -> Selections -> Map InterfaceURI FilePath -> Env -> [(InterfaceURI, Binding)] -> IO Env
doExecBindings config sels pathMap initEnv bindings = checkRunenv config >> foldM doExecBinding initEnv bindings
    where doExecBinding :: Env -> (InterfaceURI, Binding) -> IO Env
	  doExecBinding env (iface, ExecutableBinding bindingType name uncheckedCommandName) =
	    do execDir <- ensureLauncher config name
	       let commandJSON = encode (buildCommand sels env pathMap iface commandName)
	       let env2 = insert ("0install-runenv-" ++ name) commandJSON env
	       let env3 = case bindingType of
			     InVar -> insert name (execDir </> name) env2
			     InPath -> insert "PATH" newValue env2
				where oldValue = (Data.Map.lookup "PATH" env2) `mplus`
						 (standardDefault "PATH")
				      newValue = join Prepend [searchPathSeparator] oldValue execDir
	       return env3
	    where commandName = validateName uncheckedCommandName
	  doExecBinding env _ = return env

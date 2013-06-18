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

processBinding :: String -> Element -> Binding
processBinding "environment" = processEnvironmentBinding
processBinding "executable-in-path" = processExecutableBinding InPath
processBinding "executable-in-var" = processExecutableBinding InVar
processBinding n = error $ "Unknown binding type: " ++ n

isDependency :: String -> Bool
isDependency "requires" = True
isDependency "restricts" = True
isDependency "runner" = True
isDependency _ = False

isBinding :: String -> Bool
isBinding "environment"  = True
isBinding "executable-in-path" = True
isBinding "executable-in-var" = True
isBinding _ = False

-- Get bindings within a dependency
getBindingsFromDep :: Element -> [(InterfaceURI, Binding)]
getBindingsFromDep dep = do
				(name, child) <- ziChildren dep
				if isBinding (name) then
					[(iface, processBinding name child)]
				else
					[]
			where iface = requireAttr "interface" dep

-- Get bindings within a command
getBindingsFromCommand :: InterfaceURI -> Element -> [(InterfaceURI, Binding)]
getBindingsFromCommand iface commandElement = do
				(name, child) <- ziChildren commandElement
				if isBinding (name) then
					[(iface, processBinding name child)]
				else if isDependency name then
					getBindingsFromDep child
				else
					[]

-- Get all the bindings in this <selection> in document order
getBindingsFromSelection :: Element -> [(InterfaceURI, Binding)]
getBindingsFromSelection sel = do
				(name, child) <- ziChildren sel
				if isBinding name then
					return (iface, processBinding name child)
				else if isDependency name then
					getBindingsFromDep child
				else if name == "command" then
					getBindingsFromCommand iface child
				else []
		where iface = requireAttr "interface" sel

-- Find all the bindings, in document order
-- Excludes bindings to unselected optional components
collectBindings :: Selections -> [(InterfaceURI, Binding)]
collectBindings sels = do
		sel <- selectionElements
		(interfaceURI, binding) <- getBindingsFromSelection sel
		if interfaceURI `member` (selections sels) then [(interfaceURI, binding)]
		else []  -- Optional dependency which was not selected
	where selectionElements = filterChildrenName (hasZName "selection") (root sels)

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
	case maybeValue of
		Nothing -> env
		Just value -> insert name (add value) env
	where maybeValue = case bindingSource of
		    Value v -> Just v
		    InsertPath i -> case mPath of
			    Nothing -> Nothing		-- Package implementation
			    Just p -> Just $ p </> i
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

module Run where

import Text.XML.Light
import Data.Map (Map, fromList, insert, lookup)
import Data.Maybe (fromMaybe)
import System.Environment
import System.FilePath
import Control.Monad (mplus)

import Support
import Selections
import Config

type Arg = String

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
collectBindings :: Selections -> [(Selection, Binding)]
collectBindings sels = do
		sel <- selectionElements
		(interfaceURI, binding) <- getBindingsFromSelection sel
		case Data.Map.lookup interfaceURI (selections sels) of
		  Nothing -> []				-- Optional dependency which was not selected
		  Just s -> return (s, binding)
	where selectionElements = filterChildrenName (hasZName "selection") (root sels)

resolvePath :: Config -> (Selection, Binding) -> IO (Selection, Maybe FilePath, Binding)
resolvePath config (sel, binding) = do
					path <- getPath config sel
					return (sel, path, binding)

type Env = Map VarName String

join :: WhichEnd -> String -> Maybe String -> String -> String
join _ _ Nothing new = new
join Prepend sep (Just old) new = new ++ sep ++ old
join Append sep (Just old) new = old ++ sep ++ new

standardDefault :: VarName -> Maybe String
standardDefault "PATH" = Just "/bin:/usr/bin"
standardDefault "XDG_CONFIG_DIRS" = Just "/etc/xdg"
standardDefault "XDG_DATA_DIRS" = Just "/usr/local/share:/usr/share"
standardDefault _ = Nothing

doEnvBindings :: Selections -> Env -> [(Selection, Maybe FilePath, Binding)] -> Env
doEnvBindings _ env [] = env
doEnvBindings sels env ((_, path, binding) : xs) = doEnvBindings sels (doBinding binding) xs
	where doBinding (EnvironmentBinding name mode bindingSource) = doEnvBinding name mode bindingSource
	      doBinding _ = env
	      doEnvBinding name mode bindingSource = case maybeValue of
	      						Nothing -> env
							Just value -> insert name (add value) env
	      			where maybeValue = case bindingSource of
							Value v -> Just v
							InsertPath i -> case path of
								Nothing -> Nothing
								Just p -> Just $ p </> i
				      add newValue = case mode of
				      			Replace -> newValue
							Add AddMode {pos = whichEnd, defaultValue = def, separator = sep} ->
								join whichEnd sep oldValue newValue
								where oldValue = (Data.Map.lookup name env) `mplus`
										 def `mplus` (standardDefault name)


doExecBindings :: Env -> [(Selection, Maybe FilePath, Binding)] -> IO Env
doExecBindings env bindings = return env

executeSelections :: Selections -> [Arg] -> Config -> IO ()
executeSelections sels userArgs config = do
		origEnv <- getEnvironment
		pathBindings <- mapM (resolvePath config) bindings
		let env = doEnvBindings sels (fromList origEnv) pathBindings
		envWithExec <- doExecBindings env pathBindings
		print $ show envWithExec
	where bindings = collectBindings sels

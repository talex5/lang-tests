module Run where

import Text.Regex.Posix
import Text.XML.Light
import Data.Map (Map, toList, fromList, insert, lookup)
import Data.Maybe (fromMaybe, catMaybes)
import System.Environment
import System.FilePath
import System.Posix.Process (executeFile)
import Data.Text (splitOn, pack, unpack)
import Text.Printf (printf)

import Support
import Selections
import Config
import Bindings

type Arg = String

expandArg :: String -> Env -> String
expandArg template env = replaceAll re replace template
	where re = makeRegex "\\$(\\$|([a-zA-Z_][a-zA-Z0-9_]*)|\\{[^\\}]*})"
	      replace match = case match of
	      			"$$" -> "$"
				'$' : '{' : ms -> replaceVar $ init ms
				'$' : var -> replaceVar var
	      replaceVar var = case (Data.Map.lookup var env) of
	      				Nothing -> error (printf "Variable '%s' not set!" var)
					Just value -> value

expandForEach :: Element -> Env -> [String]
expandForEach parent env = do value <- values
			      arg <- argChildren
			      return $ expandArg (strContent arg) (insert "item" (unpack value) env)
	where itemFrom = requireAttr "item-from" parent
	      sep = fromMaybe [searchPathSeparator] (findAttr (mkQName "separator") parent)
	      argChildren = findChildren (QName "arg" (Just xmlns_feed) Nothing) parent
	      values = splitOn (pack sep) (pack $ fromMaybe "" $ Data.Map.lookup itemFrom env)

argsFromElem :: Element -> Env -> [String]
argsFromElem parent env = do (name, child) <- ziChildren parent
		             case name of
			          "arg" -> [expandArg (strContent child) env]
				  "for-each" -> expandForEach child env
				  _ -> []

buildCommand :: Selections -> Env -> Map InterfaceURI FilePath -> InterfaceURI -> String -> [String]
buildCommand sels env pathMap iface commandName = runnerArgs ++ commandExec ++ commandArgs
	where
		Just sel = Data.Map.lookup iface (selections sels)
		commandElem = getCommandElement commandName sel
		commandArgs = argsFromElem commandElem env
		mCommandPath = findAttr (mkQName "path") commandElem
		implPath = Data.Map.lookup iface pathMap
		commandExec = case mCommandPath of
				Nothing -> []			-- TODO: not for top-level
			        Just commandPath -> case implPath of
							Nothing -> [commandPath]		-- Native package (must be absolute)
							Just prefix -> [prefix </> commandPath]
							-- TODO: check path exists (also in OCaml)
		mRunnerElem = getRunnerElement commandElem
		runnerArgs = case mRunnerElem of
				Nothing -> []
				Just runnerElem -> (buildCommand sels env pathMap runnerIface runnerCommandName) ++ runnerExtraArgs
					      where runnerIface = requireAttr "interface" runnerElem
					      	    runnerExtraArgs = argsFromElem runnerElem env
					            runnerCommandName = fromMaybe "run" (findAttr (mkQName "command") runnerElem)

executeSelections :: Selections -> [Arg] -> Config -> IO ()
executeSelections sels userArgs config = do
		origEnv <- getEnvironment
		paths <- mapM resolvePath (toList $ selections sels)
		let pathMap = fromList $ catMaybes paths
		let env = doEnvBindings pathMap (fromList origEnv) bindings
		envWithExec <- doExecBindings pathMap env bindings
		let argv = (buildCommand sels envWithExec pathMap (interface sels) commandName) ++ userArgs
		-- print $ show envWithExec
		print $ show argv
		executeFile (head argv) False (tail argv) (Just $ toList envWithExec)
	where bindings = collectBindings sels
	      Just commandName = (command sels)
	      resolvePath (iface, sel) = do mPath <- getPath config sel
	      				    case mPath of
						    Nothing -> return Nothing
						    Just path -> return $ Just (iface, path)

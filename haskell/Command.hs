module Command where

import Text.XML.Light
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromMaybe)
import System.FilePath
import Text.Regex.Posix (makeRegex)
import Text.Printf (printf)
import Data.Text (splitOn, pack, unpack)

import Support
import Selections

expandArg :: String -> Env -> String
expandArg template env = replaceAll re replace template
	where re = makeRegex "\\$(\\$|([a-zA-Z_][a-zA-Z0-9_]*)|\\{[^\\}]*})"
	      replace match = case match of
	      			"$$" -> "$"
				'$' : '{' : ms -> replaceVar $ init ms
				'$' : var -> replaceVar var
				_ -> error "regex failed"
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


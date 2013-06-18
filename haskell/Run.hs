module Run where

import Data.Map (toList, fromList)
import Data.Maybe (catMaybes)
import System.Environment
import System.Posix.Process (executeFile)
import Command (buildCommand)

import Selections
import Config
import Bindings

type Arg = String

executeSelections :: Selections -> [Arg] -> Config -> IO ()
executeSelections sels userArgs config = do
		origEnv <- getEnvironment
		paths <- mapM resolvePath (toList $ selections sels)
		let pathMap = fromList $ catMaybes paths
		let env = foldl (doEnvBinding pathMap) (fromList origEnv) bindings
		envWithExec <- doExecBindings config sels pathMap env bindings
		let argv = (buildCommand sels envWithExec pathMap (interface sels) commandName) ++ userArgs
		-- print $ show envWithExec
		-- print $ show argv
		executeFile (head argv) False (tail argv) (Just $ toList envWithExec)
	where bindings = collectBindings sels
	      Just commandName = (command sels)
	      resolvePath (iface, sel) = do mPath <- getPath config sel
	      				    case mPath of
						    Nothing -> return Nothing
						    Just path -> return $ Just (iface, path)

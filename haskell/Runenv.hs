module Main where

import System.Environment (getArgs, getEnv, getProgName)
import System.Posix.Process (executeFile)
import Text.JSON

main = do
	envName <- getProgName
	argv <- getArgs
	jsonContents <- getEnv $ "0install-runenv-" ++ envName
	let jsv = parseJSON jsonContents
	let program:extraArgs = parseArr jsv
	executeFile program False (extraArgs ++ argv) Nothing
	where
		parseJSON str = decode str :: Result [JSString]
		parseArr :: Result [JSString] -> [String]
		parseArr (Ok jss) = map fromJSString jss

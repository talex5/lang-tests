module Main where

import System.Environment (getArgs)
import System.FilePath

import Config
import Apps
import Selections (loadSelections)
import Run (executeSelections)

main :: IO ()
main = do
	conf <- Config.get_default_config
	argv <- getArgs
	case argv of
		[] -> error "Syntax: runsels [APP | SELECTIONS] [ARGS]"
		(app_or_sels:args) -> do
			app <- lookup_app app_or_sels conf
			let selsPath = case app of
				Nothing -> app_or_sels
				Just path -> path </> "selections.xml"
			sels <- loadSelections selsPath
			executeSelections sels args conf

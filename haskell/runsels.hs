{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign
import Foreign.C

import System.Environment (getArgs)
import System.FilePath
import System.Directory (getCurrentDirectory)

import Config
import Apps
import Selections (loadSelections)
import Run (executeSelections)

main :: IO ()
main = do progPath <- getFullProgName
	  absProgPath <- absolute_path progPath
	  conf <- Config.getDefaultConfig (dropFileName absProgPath)
	  argv <- getArgs
	  case argv of
	       [] -> error "Syntax: runsels [APP | SELECTIONS] [ARGS]"
	       (appOrSels:args) -> do
			app <- lookupApp appOrSels conf
			let selsPath = case app of
				Nothing -> appOrSels
				Just path -> path </> "selections.xml"
			sels <- loadSelections selsPath
			executeSelections sels args conf

-- From http://hackage.haskell.org/trac/ghc/ticket/3199

getFullProgName :: IO String
getFullProgName =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getFullProgArgv p_argc p_argv
   peek p_argv >>= peek >>= peekCString

foreign import ccall unsafe "getFullProgArgv"
    getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
 
-- From http://hackage.haskell.org/packages/archive/MissingH/1.2.0.0/doc/html/System-Path-NameManip.html
absolute_path :: String -> IO String
absolute_path path@('/':_) = return path
absolute_path path = do
   cwd <- getCurrentDirectory
   return (cwd ++ "/" ++ path)

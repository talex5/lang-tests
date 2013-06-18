module Basedir where

import Data.Maybe (fromMaybe)
import System.FilePath
import System.Directory
import System.Posix.Files (fileExist)

import Support

type SearchPath = [FilePath]

data Basedirs = Basedirs { share :: SearchPath
			 , cache :: SearchPath
			 , config :: SearchPath
			 } deriving Show

getPath :: VarName -> VarName -> SearchPath -> IO SearchPath
getPath homeVar dirsVar defaultPath =
	case defaultPath of
	(defaultHome:defaultSystem) -> do
		userDir <- getEnvOpt homeVar
		systemDirs <- getEnvOpt dirsVar
		return $ (fromMaybe defaultHome userDir)
		       : (maybe defaultSystem splitSearchPath systemDirs)
	[] -> error "No default!"

getDefaultConfig :: IO Basedirs
getDefaultConfig = do
	home <- getHomeDirectory
	shareDir <- getPath "XDG_DATA_HOME" "XDG_DATA_DIRS" [home </> ".local/share", "/usr/local/share", "/usr/share"]
	cacheDir <- getPath "XDG_CACHE_HOME" "XDG_CACHE_DIRS" [home </> ".cache", "/var/cache"]
	configDir <- getPath "XDG_CONFIG_HOME" "XDG_CONFIG_DIRS" [home </> ".config", "/etc/xdg"]

	return $ Basedirs {
		share = shareDir,
		cache = cacheDir,
		config = configDir
	}

loadFirst :: FilePath -> SearchPath -> IO (Maybe FilePath)
loadFirst _ [] = return Nothing
loadFirst relPath (dir:xs) = do x <- fileExist fullPath
				if x then return $ Just fullPath
				else loadFirst relPath xs
		       where fullPath = dir </> relPath

save :: FilePath -> SearchPath -> IO FilePath
save relPath (dir:_) = do createDirectoryIfMissing True fullPath
			  return fullPath
		where fullPath = dir </> relPath

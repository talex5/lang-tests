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

get_path :: VarName -> VarName -> SearchPath -> IO SearchPath
get_path home_var dirs_var default_path =
	case default_path of
	(default_home:default_system) -> do
		user_dir <- getenv_opt home_var
		system_dirs <- getenv_opt dirs_var
		return $ (fromMaybe default_home user_dir)
		       : (maybe default_system splitSearchPath system_dirs)
	[] -> error "No default!"

get_default_config :: IO Basedirs
get_default_config = do
	home <- getHomeDirectory
	shareDir <- get_path "XDG_DATA_HOME" "XDG_DATA_DIRS" [home </> ".local/share", "/usr/local/share", "/usr/share"]
	cacheDir <- get_path "XDG_CACHE_HOME" "XDG_CACHE_DIRS" [home </> ".cache", "/var/cache"]
	configDir <- get_path "XDG_CONFIG_HOME" "XDG_CONFIG_DIRS" [home </> ".config", "/etc/xdg"]

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

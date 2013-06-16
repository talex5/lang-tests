module Apps where

import Text.Regex.Posix
import System.FilePath

import Config
import Basedir

valid_app_name :: String -> Bool
valid_app_name name = name =~ "^[^./\\\\:=;'\"][^/\\\\:=;'\"]*$"

lookup_app :: String -> Config -> IO (Maybe FilePath)
lookup_app name config | valid_app_name name = loadFirst ("0install.net" </> "apps" </> name) (Basedir.config $ basedirs config)
		       | True = return Nothing

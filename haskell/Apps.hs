module Apps where

import Text.Regex.Posix
import System.FilePath

import Config
import Basedir

validAppName :: String -> Bool
validAppName name = name =~ "^[^./\\\\:=;'\"][^/\\\\:=;'\"]*$"

lookupApp :: String -> Config -> IO (Maybe FilePath)
lookupApp name config | validAppName name = loadFirst ("0install.net" </> "apps" </> name) (Basedir.config $ basedirs config)
		       | True = return Nothing

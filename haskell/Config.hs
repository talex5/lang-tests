module Config where

import Basedir

data Config = Config { basedirs :: Basedir.Basedirs
		     , stores :: [String]
		     , resourceDir :: String
		     } deriving Show


getDefaultConfig :: FilePath -> IO Config
getDefaultConfig resources =
  do basedirsConfig <- Basedir.getDefaultConfig
     return Config {
	     basedirs = basedirsConfig,
	     stores = [],
	     resourceDir = resources
     }

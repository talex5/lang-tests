module Config where

import Basedir

data Config = Config { basedirs :: Basedir.Basedirs
		     , stores :: [String]
		     , resourceDir :: String
		     } deriving Show


get_default_config :: FilePath -> IO Config
get_default_config resources =
  do basedirs_config <- Basedir.get_default_config
     return Config {
	     basedirs = basedirs_config,
	     stores = [],
	     resourceDir = resources
     }

module Config where

import Basedir

data Config = Config { basedirs :: Basedir.Basedirs
		     , stores :: [String]
		     , resource_dir :: String
		     } deriving Show


get_default_config :: IO Config
get_default_config =
  do basedirs_config <- Basedir.get_default_config
     return Config {
	     basedirs = basedirs_config,
	     stores = [],
	     resource_dir = "TODO"
     }

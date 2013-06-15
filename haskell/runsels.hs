module Main where

import Basedir
import Config

main = do
	config <- Config.get_default_config
	print $ config

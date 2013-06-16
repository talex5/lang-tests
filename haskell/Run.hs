module Run where

import Selections
import Config

type Arg = String

executeSelections :: Selections -> [Arg] -> Config -> IO ()
executeSelections sels userArgs config = error (show sels)

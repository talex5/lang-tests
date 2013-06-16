module Support where

import System.Environment (getEnv)
import System.IO.Error (ioError, tryIOError, isDoesNotExistError)

xmlns_feed = "http://zero-install.sourceforge.net/2004/injector/interface"

type InterfaceURI = String
type ImplID = String
type VarName = String

getenv_opt :: VarName -> IO (Maybe String)
getenv_opt var = do
	maybe_value <- tryIOError (getEnv var)
	case maybe_value of
		Left e -> if isDoesNotExistError e then return Nothing else ioError e
		Right ok -> return $ Just ok

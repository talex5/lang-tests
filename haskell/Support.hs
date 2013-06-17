module Support where

import System.Environment (getEnv)
import System.IO.Error (tryIOError, isDoesNotExistError)
import Text.XML.Light
import Text.Printf (printf)

xmlns_feed :: String
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

mkQName :: String -> QName
mkQName name = QName name Nothing Nothing

-- For matching element names in the 0install namespace
hasZName :: String -> QName -> Bool
hasZName name qname = (qURI qname) == Just xmlns_feed && (qName qname) == name

showBrief :: Element -> String
showBrief element = printf "<%s>%s" (showQName $ elName element) location
	where location = case elLine element of
				Just l -> " at line " ++ show l
				Nothing -> ""

-- Get the value of a non-namespaced attribute
-- Report an error if it's not present
requireAttr :: String -> Element -> String
requireAttr name element = case findAttr (mkQName name) element of
	Nothing -> error $ printf "Missing required attribute '%s' on %s" name (showBrief element)
	Just x -> x

-- Get just children in the 0install namespace, returning the local name of each one
ziChildren :: Element -> [(String, Element)]
ziChildren parent = do child <- elChildren parent
		       let name = elName child
		       if ((qURI name) == Just xmlns_feed) then return ((qName name), child)
		       else []
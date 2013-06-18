module Support where

import System.Environment (getEnv)
import System.IO.Error (tryIOError, isDoesNotExistError)
import Text.XML.Light
import Text.Printf (printf)
import Text.Regex.Posix
import Data.List (foldl')
import Data.Map (Map)

xmlns_feed :: String
xmlns_feed = "http://zero-install.sourceforge.net/2004/injector/interface"

type Env = Map VarName String
type InterfaceURI = String
type ImplID = String
type VarName = String

getEnvOpt :: VarName -> IO (Maybe String)
getEnvOpt var = do
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

-- Based on http://stackoverflow.com/a/9072362/50926
-- Use the given function to replace each occurance of 're' in 's'
replaceAll :: Regex -> (String -> String) -> String -> String
replaceAll re f s = prefix end
  where (_, end, prefix) = foldl' go (0, s, id) $ getAllMatches $ match re s
        go (ind,toRead,write) (off,len) =
          let (skip, start) = splitAt (off - ind) toRead 
              (matched, remaining) = splitAt len start 
          in (off + len, remaining, write . (skip++) . (f matched ++))

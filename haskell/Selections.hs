module Selections where

import Support

import Text.Regex.Posix
import Text.Printf (printf, PrintfArg)
import Data.Map
import Text.XML.Light
import Data.ByteString (readFile)

data ImplSource = CacheSelection
		| LocalSelection FilePath
		| PackageSelection
		deriving Show

data Selection = Selection { source :: ImplSource
			   , elem2 :: Element
			   } deriving Show

data Selections = Selections { interface :: InterfaceURI
			     , command :: Maybe String
			     , selections :: Map InterfaceURI Selection
			     } deriving Show

wrapSelection :: Element -> Selection
wrapSelection node = Selection { source = source
			       , elem2 = node }
			where localPath = findAttr (qname "local-path") node
			      id = requireAttr (qname "id") node
			      source = case localPath of
			        Just path -> LocalSelection path
				Nothing -> if id =~ "^/"
					then LocalSelection id		-- Backwards compatibility
					else if id =~ "^package:"
						then PackageSelection
						else CacheSelection


loadSelections :: FilePath -> IO Selections
loadSelections path = do
	xmlStr <- Data.ByteString.readFile path			-- not plain readFile!
	let Just root = parseXMLDoc xmlStr			-- Nothing??
	let selectionElements = filterChildrenName (hasZName "selection") root
	return $ Selections { interface = requireAttr (qname "interface") root
			    , command = findAttr (qname "command") root
			    , selections = fromList $ [(requireAttr (qname "interface") sel, wrapSelection sel) | sel <- selectionElements]
	}

qname name = QName name Nothing Nothing

-- For matching element names in the 0install namespace
hasZName :: String -> QName -> Bool
hasZName name qname = (qURI qname) == Just xmlns_feed && (qName qname) == name

showBrief :: Element -> String
showBrief elem = printf "<%s>%s" (showQName $ elName elem) location
	where location = case elLine elem of
				Just l -> " at line " ++ show l
				Nothing -> ""

requireAttr name elem = case findAttr name elem of
	Nothing -> error $ printf "Missing required attribute '%s' on %s" (showQName name) (showBrief elem)
	Just x -> x

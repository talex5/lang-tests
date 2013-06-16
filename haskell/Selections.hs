module Selections where

import Support

import Text.Regex.Posix
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
			     , root :: Element
			     } deriving Show

wrapSelection :: Element -> Selection
wrapSelection node = Selection { source = source
			       , elem2 = node }
			where localPath = findAttr (mkQName "local-path") node
			      id = requireAttr "id" node
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
	return $ Selections { interface = requireAttr "interface" root
			    , command = findAttr (mkQName "command") root
			    , selections = fromList $ [(requireAttr "interface" sel, wrapSelection sel) | sel <- selectionElements]
			    , root = root
	}

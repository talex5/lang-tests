module Selections where

import Support
import Config

import Text.Regex.Posix
import Data.Map
import Text.XML.Light
import Data.ByteString (readFile)

data ImplSource = CacheSelection
		| LocalSelection FilePath
		| PackageSelection
		deriving Show

data Selection = Selection { source :: ImplSource
			   , element :: Element
			   } deriving Show

data Selections = Selections { interface :: InterfaceURI
			     , command :: Maybe String
			     , selections :: Map InterfaceURI Selection
			     , root :: Element
			     } deriving Show

wrapSelection :: Element -> Selection
wrapSelection selectionElement = Selection { source = selectionSource
					   , element = selectionElement }
			where localPath = findAttr (mkQName "local-path") selectionElement
			      implId = requireAttr "id" selectionElement
			      selectionSource = case localPath of
			        Just path -> LocalSelection path
				Nothing -> if implId =~ "^/"
					then LocalSelection implId		-- Backwards compatibility
					else if implId =~ "^package:"
						then PackageSelection
						else CacheSelection


loadSelections :: FilePath -> IO Selections
loadSelections path = do
	xmlStr <- Data.ByteString.readFile path			-- not plain readFile!
	let Just docRoot = parseXMLDoc xmlStr			-- Nothing??
	let selectionElements = filterChildrenName (hasZName "selection") docRoot
	return $ Selections { interface = requireAttr "interface" docRoot
			    , command = findAttr (mkQName "command") docRoot
			    , selections = fromList $ [(requireAttr "interface" sel, wrapSelection sel) | sel <- selectionElements]
			    , root = docRoot
	}

getPath :: Config -> Selection -> IO (Maybe FilePath)
getPath config sel = case source sel of
	CacheSelection -> return $ Just "/root"
	LocalSelection path -> return $ Just path
	PackageSelection -> return Nothing

module Selections where

import Support
import Config
import Basedir (SearchPath, cache, loadFirst)

import Text.Regex.Posix
import Data.Map
import Text.XML.Light
import Data.ByteString (readFile)
import System.FilePath

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

type Digest = (String, String)

isOldAlg :: String -> Bool
isOldAlg "sha1" = True
isOldAlg "sha1new" = True
isOldAlg "sha256" = True
isOldAlg _ = False

formatDigest :: Digest -> String
formatDigest (alg, value) = if isOldAlg alg then alg ++ "=" ++ value
			    else alg ++ "_" ++ value
			    -- TODO validate

lookupDigest :: Digest -> SearchPath -> IO (Maybe FilePath)
lookupDigest digest storesPath = loadFirst ("0install.net" </> "implementations" </> digestStr) storesPath
	where digestStr = formatDigest digest

lookupAnyDigest :: SearchPath -> [Digest] -> IO (Maybe FilePath)
lookupAnyDigest storesPath [] = error "Implementation not cached"		-- TODO: better error
lookupAnyDigest storesPath (x:xs) = do mPath <- lookupDigest x storesPath
				       case mPath of
					  Just path -> return $ Just path
					  Nothing -> lookupAnyDigest storesPath xs

getDigests :: Selection -> [Digest]
getDigests sel = do (name, child) <- ziChildren (element sel)
		    if name == "manifest-digest" then digestsFromElem child
		    else []
	where
		digestsFromElem :: Element -> [Digest]
		digestsFromElem selElement = [((qName key), value) | Attr key value <- elAttribs selElement, (qURI key) == Nothing]	-- TODO: id

getPath :: Config -> Selection -> IO (Maybe FilePath)
getPath config sel = case source sel of
	CacheSelection -> lookupAnyDigest (cache $ basedirs config) $ getDigests sel
	LocalSelection path -> return $ Just path
	PackageSelection -> return Nothing

getCommandElement :: String -> Selection -> Element
getCommandElement name (Selection _ selElem) = commandElement
	where Just commandElement = filterChild isCommand selElem
	      isCommand specimen = (qURI qname) == Just xmlns_feed && (qName qname) == "command" && (requireAttr "name" specimen) == name
	      		where qname = elName specimen

getRunnerElement :: Element -> Maybe Element
getRunnerElement commandElem = filterChildName isRunner commandElem
	where isRunner (QName { qName = "runner", qURI = Just uri }) = uri == xmlns_feed
	      isRunner _ = False

-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Image (processImages) where

import           Codec.Picture.Types (dynamicMap)
import           Codec.Picture (DynamicImage(..), imageWidth, imageHeight, readImage)
import           Control.Monad ((>=>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import           Data.List (find)
import           Data.Maybe (fromJust)
import           System.FilePath ((</>), (<.>), takeBaseName, takeFileName)
import qualified Text.HTML.TagSoup as S

import qualified Html

type Attributes = [(String, String)]

-- Returns the value of the "src" attribute.
getSrc :: Attributes -> String
getSrc = snd . fromJust . find ((== "src") . fst)

makeDimensions :: DynamicImage -> Attributes
makeDimensions img = [ ("width",  show $ dynamicMap imageWidth  img)
                     , ("height", show $ dynamicMap imageHeight img) ]

-- Given the attributes of an <img> tag, which must include the src attribute,
-- adds width and height attributes.
addDimensions :: FilePath -> Attributes -> IO Attributes
addDimensions imgDir attrs = fmap (attrs ++) dimensions
  where imgPath    = imgDir </> (takeFileName $ getSrc attrs)
        dimensions = fmap (either error makeDimensions) (readImage imgPath)

-- Maps an IO-performing function over the attributes of all <img> tags.
mapImgAttributes :: (Attributes -> IO Attributes) -> [Html.Tag] -> IO [Html.Tag]
mapImgAttributes f = mapM mapTag
  where mapTag (S.TagOpen "img" attrs) = fmap (S.TagOpen "img") $ f attrs
        mapTag otherTag                = return otherTag

-- Sets the width and height attributes of all <img> tags.
addDimensionsAll :: FilePath -> [Html.Tag] -> IO [Html.Tag]
addDimensionsAll imgDir = mapImgAttributes $ addDimensions imgDir

-- Returns file contents as a base64-encoded string.
readFileBase64 :: FilePath -> IO String
readFileBase64 = (fmap $ BSC.unpack . Base64.encode) . BS.readFile

-- Wraps a base64 string in a data url.
makePngDataUrl :: String -> String
makePngDataUrl base64 = "data:image/png;base64," ++ base64

-- Wraps base64 image contents in an inline style attribute.
makePlaceholderStyle :: String -> String
makePlaceholderStyle base64 = "background-image:url(" ++ (makePngDataUrl base64) ++ ")"

addPlaceholder :: FilePath -> Attributes -> IO Attributes
addPlaceholder placeholderDir attrs = fmap (:attrs) placeholder
  where imgPath     = placeholderDir </> (takeBaseName $ getSrc attrs) <.> "png"
        imgBase64   = readFileBase64 imgPath
        style       = fmap makePlaceholderStyle imgBase64
        placeholder = fmap (\s -> ("style", s)) style

addPlaceholdersAll :: FilePath -> [Html.Tag] -> IO [Html.Tag]
addPlaceholdersAll placeholderDir = mapImgAttributes $ addPlaceholder placeholderDir

-- Given a piece of html, inserts a placeholder background for every <img> tag,
-- and adds the image dimensions to the attributes of the tag.
processImages :: FilePath -> FilePath -> String -> IO String
processImages imgDir placeholderDir = (fmap Html.renderTags)
  . (addDimensionsAll imgDir >=> addPlaceholdersAll placeholderDir)
  . Html.parseTags

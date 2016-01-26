-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Image (processImages) where

import           Codec.Picture.Types (dynamicMap)
import           Codec.Picture (DynamicImage(..), imageWidth, imageHeight, readImage)
import           Data.List (find)
import           Data.Maybe (fromJust)
import           System.FilePath ((</>), takeFileName)
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

isImgCloseTag :: Html.Tag -> Bool
isImgCloseTag tag = case tag of
  S.TagClose "img" -> True
  _                -> False

-- Given a piece of html, inserts a placeholder background for every <img> tag,
-- and adds the image dimensions to the attributes of the tag.
processImages :: FilePath -> String -> IO String
processImages imgDir = fmap Html.renderTags
                     . addDimensionsAll imgDir
                     . filter (not . isImgCloseTag)
                     . Html.parseTags

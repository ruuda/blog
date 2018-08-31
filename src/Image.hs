-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Image (processImages) where

import           Codec.Picture (DynamicImage(..), imageWidth, imageHeight, readImage)
import           Codec.Picture.Types (dynamicMap)
import           Data.Foldable (foldrM)
import           Data.List (find, isSuffixOf, stripPrefix)
import           Data.Maybe (fromJust)
import           System.FilePath ((</>), takeFileName)
import qualified Text.HTML.TagSoup as S

import qualified Html

type Attributes = [(String, String)]

-- Returns the value of the "src" attribute.
getSrc :: Attributes -> String
getSrc attrs = case find ((== "src") . fst) attrs of
  Just (_src, value) -> value
  Nothing -> error $ "img tag without src attribute"

makeDimensions :: DynamicImage -> Attributes
makeDimensions img = [ ("width",  show $ dynamicMap imageWidth  img)
                     , ("height", show $ dynamicMap imageHeight img) ]

-- Given the attributes of an <img> tag, which must include the src attribute,
-- adds width and height attributes.
addDimensions :: FilePath -> Attributes -> IO Attributes
addDimensions imgDir attrs = fmap (attrs ++) dimensions
  where
    src        = getSrc attrs
    imgPath    = imgDir </> (takeFileName src)
    dimensions =
      if ".svg" `isSuffixOf` src
        -- Do not check the size for svg images, because Juicy Pixels cannot
        -- handle those. I could extract the size from the svg in a different
        -- way, but meh.
        then pure []
        else fmap (either error makeDimensions) (readImage imgPath)

-- Maps an IO-performing function over the attributes of all <img> tags, and
-- replaces <img> tags with an svg source with <object> tags instead, because
-- <img> tags cannot load svg images that contain stylesheets, whereas <object>
-- tags can.
mapImgAttributes :: (Attributes -> IO Attributes) -> [Html.Tag] -> IO [Html.Tag]
mapImgAttributes f = foldrM mapTag []
  where
    mapTag tag more = case tag of
      (S.TagOpen "img" attrs) | ".svg" `isSuffixOf` getSrc attrs ->
        pure $
          (S.TagOpen "object")
            [ ("type", "image/svg+xml")
            , ("data", getSrc attrs)
            , ("role", "image")
            ]
          -- Alt-text turns int content of the tag.
          : (S.TagText $ fromJust $ lookup "alt" attrs)
          : (S.TagClose "object")
          : more

      (S.TagOpen "img" attrs) | otherwise -> do
        newAttrs <- f attrs
        pure $ (S.TagOpen "img") newAttrs : more

      otherTag -> pure $ otherTag : more

-- Extract "src=" attributes from images, stripping the "/images/" prefix from
-- the path.
getSrcPaths :: [Html.Tag] -> [FilePath]
getSrcPaths tags =
  let
    appendSrc (S.TagOpen "img" attrs) srcs = getSrc attrs : srcs
    appendSrc _ srcs = srcs
  in
    fmap (fromJust . stripPrefix "/images/") $ foldr appendSrc [] tags

-- Sets the width and height attributes of all <img> tags, turn <img> tags for
-- svg images into <object> tags.
addDimensionsAll :: FilePath -> [Html.Tag] -> IO [Html.Tag]
addDimensionsAll imgDir = mapImgAttributes $ addDimensions imgDir

isImgCloseTag :: Html.Tag -> Bool
isImgCloseTag tag = case tag of
  S.TagClose "img" -> True
  _                -> False

-- Given a piece of html, adds the image dimensions to the attributes of every
-- <img> tag and ensures that there are no closing </img> tags. Returns a list
-- of referenced image file paths, and the new html.
processImages :: FilePath -> String -> IO ([FilePath], String)
processImages imgDir html =
  let
    tags = filter (not . isImgCloseTag) $ Html.parseTags html
    srcPaths = getSrcPaths tags
  in do
    newHtml <- fmap Html.renderTags $ addDimensionsAll imgDir tags
    pure (srcPaths, newHtml)

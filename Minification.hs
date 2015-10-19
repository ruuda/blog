-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Minification (minifyHtml) where

import           Data.Char (isSpace)
import qualified Text.HTML.TagSoup as S

-- Removes whitespace at the beginning and end of a string.
stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Collapses adjacent whitespace to a single whitespace character.
-- (The appended "x" zips with the last character to ensure it is not dropped.)
mergeWhitespace :: String -> String
mergeWhitespace str = fmap fst $ filter shouldKeep $ zip str $ (tail str) ++ "x"
  where shouldKeep (a, b) = not $ (isSpace a) && (isSpace b)

type Tag = S.Tag String

-- Applies the function to all tags except anything inside pre, script or style.
mapTags :: (Tag -> Tag) -> [Tag] -> [Tag]
mapTags f tags = doMap tags
  where doMap []       = []
        doMap (tag:ts) = case tag of
          S.TagOpen name _ | name `elem` preserve -> tag : (noMap ts)
          otherwise -> (f tag) : (doMap ts)
        noMap []       = []
        noMap (tag:ts) = case tag of
          S.TagClose name  | name `elem` preserve -> tag : (doMap ts)
          otherwise -> tag : (noMap ts)
        preserve = ["pre", "script", "style"]

-- Removes whitespace from tags where the whitespace can be omitted.
-- TODO: Actually, whitespace cannot be omitted everywhere here. For instance,
-- the string "a <em>b</em> c" will strip to "a<em>b</em>c", which is incorrect.
stripTags :: [Tag] -> [Tag]
stripTags = mapTags stripTag
  where stripTag tag = case tag of
          (S.TagText str) -> fmap (stripWhitespace . mergeWhitespace) tag
          otherwise       -> tag

minifyHtml :: String -> String
minifyHtml = S.renderTags . stripTags . S.parseTags

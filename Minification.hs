-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Minification (minifyHtml) where

import           Data.Char (isSpace)
import qualified Text.HTML.TagSoup as S

-- Removes the first character of a string if that character is whitespace
stripBegin :: String -> String
stripBegin []      = []
stripBegin (c:str) = if (isSpace c) then str else c:str

-- Removes the last character of a string if that character is whitespace
stripEnd :: String -> String
stripEnd = reverse . stripBegin . reverse

-- Collapses adjacent whitespace to a single whitespace character.
-- (The appended "x" zips with the last character to ensure it is not dropped.)
mergeWhitespace :: String -> String
mergeWhitespace str = fmap fst $ filter shouldKeep $ zip str $ (tail str) ++ "x"
  where shouldKeep (a, b) = not $ (isSpace a) && (isSpace b)

type Tag = S.Tag String

-- Given a set of tag names and a list of tags, produces a list where the
-- elements are the current number of unclosed tags from the set.
insideTag :: [String] -> [Tag] -> [Int]
insideTag tagNames tags = scanl nestCount 0 tags
  where nestCount n (S.TagOpen name _) | name `elem` tagNames = n + 1
        nestCount n (S.TagClose name)  | name `elem` tagNames = n - 1
        nestCount n _ = n

-- Determines for every tag whether it is inside a tag that might have
-- significant whitespace.
insidePre :: [Tag] -> [Bool]
insidePre = fmap (> 0) . insideTag ["pre"]

-- Applies a mapping function to the tags, except when a tag is inside a tag a
-- tag that might have significant whitespace. The function `tmap` is a way to
-- abstract over the mapping function, it should not alter the length of the
-- list.
applyTagsExcept :: ([Tag] -> [Tag]) -> [Tag] -> [Tag]
applyTagsExcept tmap tags = fmap select $ zip3 (insidePre tags) tags (tmap tags)
  where select (inPre, orig, mapped) = if inPre then orig else mapped

mapWithPrevious :: (Maybe a -> a -> b) -> [a] -> [b]
mapWithPrevious f xs = fmap (uncurry f) $ zip (Nothing : fmap Just xs) xs

mapWithNext :: (a -> Maybe a -> b) -> [a] -> [b]
mapWithNext f xs = fmap (uncurry f) $ zip xs ((tail $ fmap Just xs) ++ [Nothing])

-- Applies f to all tags except when the tag is inside a tag in `preTags`.
mapTagsExcept :: (Tag -> Tag) -> [Tag] -> [Tag]
mapTagsExcept f = applyTagsExcept $ fmap f

-- Applies f to all tags and their predecessors, except inside a tag in `preTags`.
mapTagsPreviousExcept :: (Maybe Tag -> Tag -> Tag) -> [Tag] -> [Tag]
mapTagsPreviousExcept f = applyTagsExcept $ mapWithPrevious f

-- Applies f to all tags and their successors, except inside a tag in `preTags`.
mapTagsNextExcept :: (Tag -> Maybe Tag -> Tag) -> [Tag] -> [Tag]
mapTagsNextExcept f = applyTagsExcept $ mapWithNext f

-- Applies a function to the text of a text tag.
mapText :: (String -> String) -> Tag -> Tag
mapText f (S.TagText str) = S.TagText (f str)
mapText f tag             = tag

-- Applies a function to the text of a tag if the other tag exists and
-- satisfies a condition.
mapTextIf :: (Tag -> Bool) -> Maybe Tag -> (String -> String) -> Tag -> Tag
mapTextIf cond (Just other) f tag = if (cond other) then mapText f tag else tag
mapTextIf cond Nothing      f tag = tag

-- Strips whitespace after an opening tag.
stripAfterOpen :: Maybe Tag -> Tag -> Tag
stripAfterOpen prev tag = mapTextIf S.isTagOpen prev stripBegin tag

-- Strips whitespace before a closing tag.
stripBeforeClose :: Tag -> Maybe Tag -> Tag
stripBeforeClose tag next = mapTextIf S.isTagClose next stripEnd tag

-- Removes excess whitespace. Whitespace is removed in the following places:
--
--  * After an opening tag.
--  * Before a closing tag.
--  * Between the closing tag and opening tag of non-inline tags. (TODO)
--
--  Consecutive whitespace is merged to a single whitespace character.
--  Whitespace inside <pre> is left untouched.
stripTags :: [Tag] -> [Tag]
stripTags =
  -- TODO: filter comment tags.
  -- TODO: filter between tags.
  (mapTagsNextExcept stripBeforeClose) .
  (mapTagsPreviousExcept stripAfterOpen) .
  (mapTagsExcept $ mapText mergeWhitespace)

minifyHtml :: String -> String
minifyHtml = S.renderTags . stripTags . S.parseTags

-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Minification (minifyCss, minifyHtml) where

import           Data.Char (isSpace)
import qualified Text.HTML.TagSoup as S

import qualified Html

type Tag = Html.Tag

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

mapWithPrevious :: (Maybe a -> a -> b) -> [a] -> [b]
mapWithPrevious f xs = fmap (uncurry f) $ zip (Nothing : fmap Just xs) xs

mapWithNext :: (a -> Maybe a -> b) -> [a] -> [b]
mapWithNext f xs = fmap (uncurry f) $ zip xs ((tail $ fmap Just xs) ++ [Nothing])

filterWithPrevious :: (Maybe a -> a -> Bool) -> [a] -> [a]
filterWithPrevious f xs = fmap snd . filter (uncurry f) $ zip (Nothing : fmap Just xs) xs

filterWithNext :: (a -> Maybe a -> Bool) -> [a] -> [a]
filterWithNext f xs = fmap fst . filter (uncurry f) $ zip xs ((tail $ fmap Just xs) ++ [Nothing])

-- Determines for every character whether it is inside a /* */ comment.
identifyComments :: String -> [Bool]
identifyComments = identify False
  where identify _ ('/' : '*' : more) = True : True : (identify True more)
        identify _ ('*' : '/' : more) = True : True : (identify False more)
        identify s (x : xs)           = s : (identify s xs)
        identify _ []                 = []

-- Removes /* */ comments.
stripCssComments :: String -> String
stripCssComments css = fmap fst $ filter (not . snd) $ zip css (identifyComments css)

-- Removes whitespace after a colon, semicolon, comma, curly brackets,
-- or after parentheses.
stripCssAfter :: String -> String
stripCssAfter = filterWithPrevious shouldKeep
  where shouldKeep (Just p) c = not $ (isSpace c) && (p `elem` ",:;{}()")
        shouldKeep _ _        = True

-- Removes whitespace before a curly bracket, and the last semicolon before a
-- closing bracket.
stripCssBefore :: String -> String
stripCssBefore = filterWithNext shouldKeep
  where shouldKeep s   (Just '{') = not $ isSpace s
        shouldKeep ';' (Just '}') = False
        shouldKeep _ _            = True

-- A basic css minifier that merges and removes whitespace. The transformations
-- it makes might not be correct (inside strings for example), but it works for
-- the stylesheets that I use it on.
minifyCss :: String -> String
minifyCss = stripBegin . stripEnd
          . stripCssBefore . stripCssAfter
          . mergeWhitespace . stripCssComments

-- Applies f to all tags except when the tag is inside a <pre> tag.
applyTagsExceptPre :: ([Tag] -> [Tag]) -> [Tag] -> [Tag]
applyTagsExceptPre = Html.applyTagsWhere (not . Html.isPre)

-- Applies f to all tags except when the tag is inside a <pre> tag.
mapTagsExceptPre :: (Tag -> Tag) -> [Tag] -> [Tag]
mapTagsExceptPre = Html.mapTagsWhere (not . Html.isPre)

-- Applies f to all tags and their predecessors, except inside a <pre> tag.
mapTagsPreviousExceptPre :: (Maybe Tag -> Tag -> Tag) -> [Tag] -> [Tag]
mapTagsPreviousExceptPre f = applyTagsExceptPre $ mapWithPrevious f

-- Applies f to all tags and their successors, except inside a <pre> tag.
mapTagsNextExceptPre :: (Tag -> Maybe Tag -> Tag) -> [Tag] -> [Tag]
mapTagsNextExceptPre f = applyTagsExceptPre $ mapWithNext f

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

-- Strips whitespace before an opening tag if the tag is not inline.
stripBeforeOpen :: Tag -> Maybe Tag -> Tag
stripBeforeOpen tag next = mapTextIf shouldStripBefore next stripEnd tag
  where shouldStripBefore (S.TagOpen name _) = not $ isInline name
        shouldStripBefore _ = False

-- Strips whitespace after a closing tag if the tag is not inline.
stripAfterClose :: Maybe Tag -> Tag -> Tag
stripAfterClose prev tag = mapTextIf shouldStripAfter prev stripBegin tag
  where shouldStripAfter (S.TagClose name) = not $ isInline name
        shouldStripAfter _ = False

-- Tests whether an element is inline. The list here is not exhaustive. The
-- elements have been chosen such that significant whitespace is not removed
-- from the html that I feed through the minifier (my rendered blog posts).
isInline :: String -> Bool
isInline t = t `elem` ["a", "acronym", "code", "em", "math", "span", "strong", "time"]

-- Removes comment tags and merges adjacent text tags.
removeComments :: [Tag] -> [Tag]
removeComments = merge . filter (not . S.isTagComment)
  where merge (S.TagText u : S.TagText v : more) = merge $ (S.TagText $ u ++ v) : more
        merge (tag : more) = tag : (merge more)
        merge [] = []

-- Minifies the contents of all <style> tags.
minifyStyleTags :: [Tag] -> [Tag]
minifyStyleTags = Html.mapTagsWhere Html.isStyle $ mapText minifyCss

-- Removes excess whitespace and comments. Whitespace is removed in the
-- following places:
--
--  * After an opening tag.
--  * Before a closing tag.
--  * Before the opening tag of a non-inline element.
--  * After the closing tag of a non-inline element.
--
--  Consecutive whitespace is merged to a single whitespace character.
--  Whitespace inside <pre> is left untouched.
stripTags :: [Tag] -> [Tag]
stripTags =
  (mapTagsPreviousExceptPre stripAfterClose) .
  (mapTagsNextExceptPre stripBeforeOpen) .
  (mapTagsNextExceptPre stripBeforeClose) .
  (mapTagsPreviousExceptPre stripAfterOpen) .
  (mapTagsExceptPre $ mapText mergeWhitespace) .
  (removeComments)

-- Minifies html by removing excess whitespace and comments, and by minifying
-- inline stylesheets.
minifyHtml :: String -> String
minifyHtml = Html.renderTags . minifyStyleTags . stripTags . Html.parseTags

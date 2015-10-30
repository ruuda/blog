-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Font (getCode, getEmText, getStrongText, getCodeGlyphs) where

import           Control.Monad (join)
import           Data.Char (isAscii, isLetter)
import           Data.List (intersperse)
import qualified Data.Set as Set
import qualified Text.HTML.TagSoup as S

import           Html (Tag, insideTag)

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

-- Returns the the text in all tags with the specified name.
getTextInTag :: String -> String -> String
getTextInTag name = join . intersperse " " . getText . filterInside . S.parseTags
  where inside       tags = fmap (> 0) $ insideTag [name] tags
        filterInside tags = fmap fst $ filter snd $ zip tags (inside tags)
        getText           = fmap S.fromTagText . filter S.isTagText

-- Extracts all text between <code> tags.
getCode :: String -> String
getCode = getTextInTag "code"

-- Extracts all text between <em> tags.
getEmText :: String -> String
getEmText = getTextInTag "em"

-- Extracts all text between <strong> tags.
getStrongText :: String -> String
getStrongText = getTextInTag "strong"

-- Convert a unicode character to its postscript glyph name.
getGlyphName :: Char -> String
getGlyphName c = case c of
  a | (isAscii a) && (isLetter a) -> [a] -- Ascii letters are their own name.
  '\\' -> "backslash"
  '\'' -> "quotesingle"
  ' ' -> "space"
  '!' -> "exclam"
  '"' -> "quotedbl"
  '#' -> "numbersign"
  '$' -> "dollar"
  '%' -> "percent"
  '&' -> "ampersand"
  '(' -> "parenleft"
  ')' -> "parenright"
  '*' -> "asterisk"
  '+' -> "plus"
  ',' -> "comma"
  '-' -> "hyphen"
  '.' -> "period"
  '/' -> "slash"
  '0' -> "zero"
  '1' -> "one"
  '2' -> "two"
  '3' -> "three"
  '4' -> "four"
  '5' -> "five"
  '6' -> "six"
  '7' -> "seven"
  '8' -> "eight"
  '9' -> "nine"
  ':' -> "colon"
  ';' -> "semicolon"
  '<' -> "less"
  '=' -> "equal"
  '>' -> "greater"
  '?' -> "question"
  '[' -> "bracketleft"
  ']' -> "bracketright"
  '_' -> "underscore"
  '`' -> "grave"
  '{' -> "braceleft"
  '|' -> "bar"
  '}' -> "braceright"
  'é' -> "eacute"
  'ë' -> "edieresis"
  '‘' -> "quoteleft"
  '’' -> "quoteright"

-- Returns a list of postscript glyph names required to typeset the content of
-- all <code> tags in the string.
getCodeGlyphs :: String -> [String]
getCodeGlyphs = fmap getGlyphName . filter (/= '\n') . unique . getCode

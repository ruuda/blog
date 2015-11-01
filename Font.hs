-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Font (getCodeGlyphs) where

import           Data.Char (isAscii, isLetter)
import qualified Data.Set as Set

import qualified Html

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

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
getCodeGlyphs = fmap getGlyphName . filter (/= '\n') . unique . Html.getCode

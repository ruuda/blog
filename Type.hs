-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Type ( SubsetCommand
            , getCode
            , getEmText
            , getStrongText
            , subsetArtifact
            , subsetFonts
            ) where

import           Control.Monad (mapM)
import           Data.Char (isAscii, isLetter)
import qualified Data.Set as Set
import           System.IO (hClose, hPutStrLn)
import qualified System.Process as P

import qualified Html

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

-- Extracts all text between <code> tags.
getCode :: String -> String
getCode = Html.getTextInTag Html.isCode

-- Extracts all text between <em> tags.
getEmText :: String -> String
getEmText = Html.getTextInTag Html.isEm

-- Extracts all text between <strong> tags.
getStrongText :: String -> String
getStrongText = Html.getTextInTag Html.isStrong

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

-- Returns a list of postscript glyph names required to typeset the content of
-- all italic text in a post. (The text between <em> tags.)
-- TODO: Take ligatures into account.
getItalicGlyphs :: String -> [String]
getItalicGlyphs = fmap getGlyphName . filter (/= '\n') . unique . getEmText

-- A subset command is the source font filename, the destination basename, and
-- the glyph names of the glyphs to subset.
data SubsetCommand = SubsetCommand FilePath FilePath [String] deriving (Show)

subsetFonts :: [SubsetCommand] -> IO ()
subsetFonts commands = do
  (Just stdin, mstdout, mstderr, pid) <- P.createProcess subsetScriptPiped
  mapM (pushCommand stdin) commands
  hClose stdin
  P.waitForProcess pid
  return () -- Ignore the exit code.
  where subsetScript = P.proc "python3" ["fonts/subset.py"]
        -- The Python interpreter needs to have a pipe for stdin because we
        -- want to write to it.
        subsetScriptPiped = subsetScript { P.std_in = P.CreatePipe }
        pushCommand stdin (SubsetCommand src dst glyphs) = do
          hPutStrLn stdin src
          hPutStrLn stdin dst
          hPutStrLn stdin $ unwords glyphs

-- Given font file basename and html contents, generates subset commands that
-- will output subsetted fonts with the given basename, and a suffix:
--
--  * "m" for monospace. (Subset of Inconsolata.)
--  * "i" for italic. (Subset of Calluna Sans.)
--  * TODO: subset others too.
--
--  Both a woff and woff2 file will be written.
subsetArtifact :: FilePath -> String -> [SubsetCommand]
subsetArtifact baseName html = filter isUseful commands
  where isUseful (SubsetCommand _ _ glyphs) = not $ null glyphs
        italicGlyphs  = getItalicGlyphs html
        monoGlyphs    = getCodeGlyphs html
        italicCommand = SubsetCommand "fonts/calluna-sans-italic.otf" (baseName ++ "i") italicGlyphs
        monoCommand   = SubsetCommand "fonts/inconsolata.otf" (baseName ++ "m") monoGlyphs
        commands      = [italicCommand, monoCommand]

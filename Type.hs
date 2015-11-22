-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Type ( SubsetCommand
            , expandPunctuation
            , getCode
            , getEmText
            , getStrongText
            , subsetArtifact
            , subsetFonts
            ) where

import           Data.Char (isAscii, isLetter, isSpace)
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
  '\x2009' -> "thinspace"
  '\x2013' -> "endash"
  '\x2014' -> "emdash"
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
  _   -> error $ "no postscript glyph name for '" ++ [c] ++ "'"

-- Given a piece of text, returns the glyph names of the ligatures required to
-- typeset the text.
getLigatures :: String -> [String]
getLigatures = buildList []
  where buildList glyphs str =
          let liga more ligaName = buildList (ligaName : glyphs) more in
          case str of
            -- In general, the postscript glyph name of a ligature is made up
            -- of the letters separated by underscores. However, ff, fi, and
            -- fl are an exception to this rule.
            []             -> glyphs
            'f':'f':'b':xs -> liga xs "f_f_b"
            'f':'f':'h':xs -> liga xs "f_f_h"
            'f':'f':'i':xs -> liga xs "f_f_i"
            'f':'f':'j':xs -> liga xs "f_f_j"
            'f':'f':'k':xs -> liga xs "f_f_k"
            'f':'f':'l':xs -> liga xs "f_f_l"
            'f':'b':xs     -> liga xs "f_b"
            'f':'f':xs     -> liga xs "ff"
            'f':'h':xs     -> liga xs "f_h"
            'f':'i':xs     -> liga xs "fi"
            'f':'j':xs     -> liga xs "f_j"
            'f':'k':xs     -> liga xs "f_k"
            'f':'l':xs     -> liga xs "fl"
            _ : xs         -> buildList glyphs xs

data IncludeLigatures = NoLigatures
                      | WithLigatures
                      | WithDiscretionaryLigatures

-- Given a piece of text, returns a list of postscript glyph names required to
-- typeset the text.
getGlyphs :: IncludeLigatures -> String -> [String]
getGlyphs ligatures str = case ligatures of
  NoLigatures                -> glyphs
  WithLigatures              -> glyphs ++ ligaGlyphs
  WithDiscretionaryLigatures -> glyphs ++ ligaGlyphs ++ dligGlyphs
  where
    glyphs     = fmap getGlyphName $ filter (/= '\n') $ unique str
    ligaGlyphs = unique $ getLigatures str
    dligGlyphs = [] -- TODO: extract discretionary ligatures

-- Returns a list of postscript glyph names required to typeset the content of
-- all <code> tags in the string.
getCodeGlyphs :: String -> [String]
getCodeGlyphs = getGlyphs NoLigatures . getCode

-- Returns a list of postscript glyph names required to typeset the content of
-- all italic body text in a post. (The text between <em> tags.)
getItalicGlyphs :: String -> [String]
getItalicGlyphs = getGlyphs WithLigatures . getEmText

-- Returns a list of postscript glyph names required to typeset the content of
-- all bold body text in a post. (The text between <strong> tags.)
getBoldGlyphs :: String -> [String]
getBoldGlyphs = getGlyphs WithLigatures . getStrongText

-- A subset command is the source font filename, the destination basename, and
-- the glyph names of the glyphs to subset.
data SubsetCommand = SubsetCommand FilePath FilePath [String] deriving (Show)

subsetFonts :: [SubsetCommand] -> IO ()
subsetFonts commands = do
  (Just stdin, _stdout, _stderr, pid) <- P.createProcess subsetScriptPiped
  mapM_ (pushCommand stdin) commands
  hClose stdin
  _exitCode <- P.waitForProcess pid -- Ignore the exit code.
  return ()
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
        boldGlyphs    = getBoldGlyphs html
        italicGlyphs  = getItalicGlyphs html
        monoGlyphs    = getCodeGlyphs html
        boldCommand   = SubsetCommand "fonts/calluna-sans-bold.otf" (baseName ++ "b") boldGlyphs
        italicCommand = SubsetCommand "fonts/calluna-sans-italic.otf" (baseName ++ "i") italicGlyphs
        monoCommand   = SubsetCommand "fonts/inconsolata.otf" (baseName ++ "m") monoGlyphs
        commands      = [boldCommand, italicCommand, monoCommand]

-- Replaces double dashes (--) surrounded by spaces with em-dashes (—)
-- surrounded by thin spaces, and single dashes surrounded by spaces with
-- en-dashes surrounded by thin spaces. Also replaces triple dots with an
-- ellipsis (…).
expandPunctuationRaw :: String -> String
expandPunctuationRaw str = case str of
  -- The code point U+2009 is a (breakable) thin space. The code point U+2014
  -- is an em-dash (—), U+2013 an en-dash (–). Though they can be embedded in
  -- string literals directly, they are escaped because they can be hard to
  -- distinguish in an editor with monospace font.
  s1:'-':'-':s2:more -> if isSpace s1 && isSpace s2
                          then "\x2009\x2014\x2009" ++ expandPunctuationRaw more
                          else s1 : '-' : '-' : s2 : expandPunctuationRaw more
  s1:'-':s2:more     -> if isSpace s1 && isSpace s2
                          then "\x2009\x2013\x2009" ++ expandPunctuationRaw more
                          else s1 : '-' : s2 : expandPunctuationRaw more
  '.':'.':'.':more   -> '…' : expandPunctuationRaw more
  c:more             -> c : expandPunctuationRaw more
  []                 -> []

-- Expands punctuation like expandPunctuationRaw in the html body, except in
-- tags where this is invalid (in <code> tags).
expandPunctuation :: String -> String
expandPunctuation = Html.renderTags . Html.mapTagsWhere inBody expand . Html.parseTags
  where inBody    = not . Html.isCode
        expand    = Html.mapText expandPunctuationRaw

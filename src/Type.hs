-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Type ( SubsetCommand
            , expandPunctuation
            , makeAbbrs
            , subsetArtifact
            , subsetFonts
            , usesBoldFont
            , usesSerifItalicFont
            ) where

import Control.Monad (filterM)
import Data.Bits (rotateL, xor)
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isLetter, isSpace, ord, toLower)
import Data.Foldable (foldl')
import Data.Maybe (fromJust, isJust)
import Data.Word (Word32, Word64)
import Numeric (showHex)
import System.Directory (doesFileExist)
import System.IO (hClose, hPutStrLn)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Process as P
import qualified Text.HTML.TagSoup as S

import qualified Html
import qualified Template

type TagProperties = Html.TagProperties

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

data FontFamily  = Mono | Sans | Serif                      deriving Eq
data FontWeight  = Regular | Bold                           deriving Eq
data FontStyle   = Roman | Italic                           deriving Eq
data FontCaps    = UnchangedCaps | SmallCaps | AllSmallCaps deriving Eq
type FontAndCaps = (FontFamily, FontWeight, FontStyle, FontCaps)
type Font        = (FontFamily, FontWeight, FontStyle)

-- Whether a tag needs typesetting with a webfont.
needsFont :: TagProperties -> Bool
needsFont t = case t of
  _ | Html.isHead t   -> False
  _ | Html.isScript t -> False
  _ | Html.isStyle t  -> False
  _ | otherwise       -> True

getFamily :: TagProperties -> FontFamily
getFamily t = case t of
  _ | Html.isCode t        -> Mono
  _ | Html.isH3 t          -> Sans
  _ | Html.isVar t         -> Sans
  _ | Html.isBlockQuote t  -> Serif
  _ | Html.isArchiveLink t -> Serif
  _ | Html.isHeader t      -> Serif
  _ | Html.isHeading t     -> Serif
  _ | Html.isTeaserLink t  -> Serif
  _ | otherwise            -> Sans

getWeight :: TagProperties -> FontWeight
getWeight t = case t of
  _ | Html.isSubtitle t -> Regular
  _ | Html.isHeading t  -> Bold
  _ | Html.isStrong t   -> Bold
  _ | Html.isTh t       -> Bold
  _ | otherwise         -> Regular

getStyle :: TagProperties -> FontStyle
getStyle t = case t of
  _ | Html.isBlockQuote t -> Italic
  _ | Html.isEm t         -> Italic
  _ | Html.isSubtitle t   -> Italic
  _ | Html.isVar t        -> Italic
  _ | otherwise           -> Roman

getCaps :: TagProperties -> FontCaps
getCaps t = case t of
  _ | Html.isAbbr t       -> AllSmallCaps
  _ | Html.isSmcp t       -> AllSmallCaps
  _ | Html.isRunIn t      -> SmallCaps
  _ | otherwise           -> UnchangedCaps

getFont :: TagProperties -> Maybe FontAndCaps
getFont t = if needsFont t then Just font else Nothing
  where font = (getFamily t, getWeight t, getStyle t, getCaps t)

-- Splits a string into words at spaces, dashes and dots. Does not discard any
-- characters, split points become single-character elements.
splitWords :: String -> [String]
splitWords = filter (not . null) . foldr prepend [""]
  where prepend _ []          = error "unreachable"
        prepend c (word:more) = if not $ isLetter c
                                then "" : [c] : word : more
                                else (c : word) : more

data AbbrWord = MixedCaps String | AllCaps String

abbrNull :: AbbrWord -> Bool
abbrNull (MixedCaps str) = null str
abbrNull (AllCaps   str) = null str

-- Splits a string into sentences and all-caps words alternatingly.
splitAbbrs :: String -> [AbbrWord]
splitAbbrs = filter (not . abbrNull) . foldr prepend [MixedCaps ""] . splitWords
  where prepend _    []              = error "unreachable"
        prepend word (AllCaps   str : more) = MixedCaps word : AllCaps str : more
        prepend word (MixedCaps str : more) = if (all isAsciiUpper word) && (length word >= 2)
                                            then AllCaps word : MixedCaps str : more
                                            else MixedCaps (word ++ str) : more

-- Inserts <abbr> tags around words in all-caps.
makeAbbrs :: String -> String
makeAbbrs = Html.renderTags . Html.concatMapTagsWhere isBodyTag mkAbbr . Html.parseTags
  where mkAbbr (S.TagText str)      = concatMap insertAbbrs $ splitAbbrs str
        mkAbbr tag                  = [tag]
        insertAbbrs (MixedCaps str) = [S.TagText str]
        insertAbbrs (AllCaps   str) = [S.TagOpen "abbr" [], S.TagText str, S.TagClose "abbr"]
        -- Only insert <abbr> tags in things that are typesetted (text content),
        -- but not in monospace content (code). Also not when we are already
        -- inside an <abbr> tag.
        isBodyTag t = (needsFont t) && (not $ Html.isCode t) && (not $ Html.isAbbr t)

-- Returns whether Calluna or Inconsolata has a glyph for the character. This
-- function is optimistic, so getGlyphName still fails for unexpected glyphs.
isGlyphSupported :: Char -> Bool
isGlyphSupported c = not $ c `elem` ['\n', 'φ', 'ψ', '≡']

-- Convert a unicode character to its postscript glyph name.
getGlyphName :: Char -> String
getGlyphName c = case c of
  a | (isAscii a) && (isLetter a) -> [a] -- Ascii letters are their own name.
  '\x00a0' -> "uni00A0"                  -- U+00A0 is a non-breaking space.
  '\x2009' -> "thinspace"                -- TODO: Should I remove it, and put
  '\x2013' -> "endash"                   -- an &nbsp; in the source, just to
  '\x2014' -> "emdash"                   -- keep the source free of magic?
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
  '≤' -> "lessequal"
  '=' -> "equal"
  '≥' -> "greaterequal"
  '>' -> "greater"
  '?' -> "question"
  '@' -> "at"
  '[' -> "bracketleft"
  ']' -> "bracketright"
  '^' -> "asciicircum"
  '_' -> "underscore"
  '`' -> "grave"
  '{' -> "braceleft"
  '|' -> "bar"
  '}' -> "braceright"
  '⌈' -> "uni2308"
  '⌉' -> "uni2309"
  '⌊' -> "uni230A"
  '⌋' -> "uni230B"
  '∎' -> "qed"
  '~' -> "asciitilde"
  '±' -> "plusminus"
  '¶' -> "paragraph"
  '·' -> "periodcentered"
  '»' -> "guillemotright" -- A typo in the postscript specification.
  'à' -> "agrave"
  'é' -> "eacute"
  'ë' -> "edieresis"
  'μ' -> "mu" -- For some reason called  uni03C2 in Minion, but I just call it mu.
  'σ' -> "sigma"
  '√' -> "radical"
  '‘' -> "quoteleft"
  '’' -> "quoteright"
  '“' -> "quotedblleft"
  '”' -> "quotedblright"
  '•' -> "bullet"
  '…' -> "ellipsis"
  '≈' -> "approxequal"
  '≠' -> "notequal"
  '≰' -> "notlessnorequal"
  '𝔽' -> "u1D53D"
  _   -> error $ "no postscript glyph name for '" ++ [c] ++ "' " ++
                 "(code point " ++ (show $ ord c) ++ ")"

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

-- Given a piece of text, returns the postscript glyph names of the
-- discretionary ligatures required to typeset the text.
getDiscretionaryLigatures :: String -> [String]
getDiscretionaryLigatures = buildList []
  where buildList glyphs str =
          let liga more ligaName = buildList (ligaName : glyphs) more in
          case str of
            -- As with the normal ligatures, the glyph names are inconsistent.
            -- I can turn these on and off with a <span class="dlig">, some of
            -- them are usually too intense for my taste, like the ip and it
            -- ligatures.
            []             -> glyphs
            'c':'b':xs     -> liga xs "c_b"
            'c':'h':xs     -> liga xs "c_h"
            'c':'k':xs     -> liga xs "c_k"
            'c':'p':xs     -> liga xs "c_p"
            'c':'t':xs     -> liga xs "ct"
            'g':'i':xs     -> liga xs "g_i"
            'i':'t':xs     -> liga xs "i_t"
            'q':'u':xs     -> liga xs "q_u"
            's':'b':xs     -> liga xs "s_b"
            's':'h':xs     -> liga xs "s_h"
            's':'k':xs     -> liga xs "s_k"
            's':'p':xs     -> liga xs "s_p"
            's':'t':xs     -> liga xs "st"
            _ : xs         -> buildList glyphs xs

-- Parses html and returns a list with pieces of text and the font they should
-- be set in.
mapFont :: String -> [(String, FontAndCaps)]
mapFont = (fmap dropMaybe)
        . (filter $ isJust . snd)
        . (fmap selectFont)
        . (filter $ S.isTagText . fst)
        . Html.classifyTags
        . Html.parseTags
  where dropMaybe  (str, justFont) = (str, fromJust justFont)
        selectFont (tag, props)    = (S.fromTagText tag, getFont props)

-- Given html, returns a string that contains the list numbers.
synthesizeListNumbers :: String -> String
synthesizeListNumbers html = concatMap show [1 .. Html.maxOlLength html]

-- Given html, returns a list bullet if the html contains an unordered list.
synthesizeListBullets :: String -> String
synthesizeListBullets html = if Html.hasUl html then "•" else ""

-- Given html, returns a pilcrow bullet if the html contains a h2 in body.
synthesizePilcrow :: String -> String
synthesizePilcrow html = if Html.hasH2 html then "¶" else ""

-- Given html, returns a string with guillemets that are added by css.
synthesizeGuillemets :: String -> String
synthesizeGuillemets html = if hasMoreLink then "\x00a0»" else ""
  where hasMoreLink  = not $ null
                     $ filter (\t -> Html.isTeaserLink t || Html.isArchiveLink t)
                     $ fmap snd
                     $ Html.classifyTags
                     $ Html.parseTags html

-- Returns the text that is not present in the html but generated by css.
synthesizeFont :: String -> [(String, FontAndCaps)]
synthesizeFont html = filter (not . null . fst)
  [ (synthesizeListNumbers html, (Sans,  Bold,    Roman, UnchangedCaps))
  , (synthesizeListBullets html, (Sans,  Regular, Roman, UnchangedCaps))
  , (synthesizeGuillemets  html, (Serif, Regular, Roman, AllSmallCaps ))
  , (synthesizePilcrow     html, (Serif, Bold,    Roman, UnchangedCaps))]

-- Returns all the text that should be typeset and the font it should be set in.
mapFontFull :: String -> [(String, FontAndCaps)]
mapFontFull html = (mapFont html) ++ (synthesizeFont html)

data IncludeLigatures = NoLigatures
                      | WithLigatures
                      | WithDiscretionaryLigatures

-- Given a font and whether to include ligatures, and content processed by
-- mapFont, returns a list of postscript glyph names required to typeset the
-- content.
getGlyphs :: Font -> IncludeLigatures -> [(String, FontAndCaps)] -> [String]
getGlyphs font ligatures = unique . (concatMap mapGlyphs) . (filter matchesFont)
        -- Note: there are more small cap glyphs than just the letters, but for
        -- now I don't use them.
        -- For a small cap, include both the small cap and the regular letter.
        -- Because of the way opentype works, the capital (for c2sc) or
        -- lowercase letter (for smcp) needs to be included even if it is not
        -- used, because small caps follow a substitution rule, and if the true
        -- glyph is not there, there is nothing to substitute.
  where makeSmcp predicate glyph = case glyph of
          g:[] | predicate g -> [(toLower g) : ".smcp", glyph]
          _                  -> [glyph]
        glyphsFor      = fmap getGlyphName . filter isGlyphSupported . unique
        ligasFor str   = case ligatures of
          NoLigatures -> []
          _           -> getLigatures str
        dligsFor str   = case ligatures of
          WithDiscretionaryLigatures -> getDiscretionaryLigatures str
          _                          -> []
        matchesFont  (_,   (f, w, s, _   )) = (font == (f, w, s))
        mapGlyphs    (str, (_, _, _, caps)) = case caps of
          UnchangedCaps -> (glyphsFor str) ++ (ligasFor str) ++ (dligsFor str)
          SmallCaps     -> concatMap (makeSmcp isAsciiLower) $ glyphsFor str
          AllSmallCaps  -> concatMap (makeSmcp isAscii     ) $ glyphsFor str

-- A subset command is the source font filename, the destination basename, and
-- the glyph names of the glyphs to subset.
data SubsetCommand = SubsetCommand FilePath FilePath [String] deriving (Show)

-- Execute the subset commands against a pool of subsetting processes. Returns
-- the number of subsetted fonts that did not exist yet.
subsetFonts :: [SubsetCommand] -> IO Int
subsetFonts commands = do
  -- The subsetted fonts are immutable, with content-based filenames. This means
  -- that if the file exists already, there is no need to re-generate it,
  -- because the contents will be the same. So filter out existing files.
  let doesExist (SubsetCommand _ dest _) = doesFileExist (dest ++ ".woff")
  commandsNew <- filterM (fmap not . doesExist) commands
  -- Divide the workload over eight processes to speed up subsetting.
  procs <- sequence $ replicate 8 $ P.createProcess subsetScriptPiped
  let stdins = fmap (\(Just stdin, _, _, _) -> stdin) procs
      pids   = fmap (\(_, _, _, pid) -> pid) procs
  mapM_ (uncurry pushCommand) (zip (cycle stdins) commandsNew)
  mapM_ hClose stdins
  mapM_ P.waitForProcess pids -- Wait, but ignore the exit codes.
  pure $ length commandsNew
  where
    -- When run from the Nix profile, Python has access to the right
    -- dependencies.
    subsetScript = P.proc "/usr/bin/env" ["python3", "fonts/subset.py"]
    -- The Python interpreter needs to have a pipe for stdin because we
    -- want to write to it.
    subsetScriptPiped = subsetScript { P.std_in = P.CreatePipe }
    pushCommand stdin (SubsetCommand src dst glyphs) = do
      hPutStrLn stdin src
      hPutStrLn stdin dst
      hPutStrLn stdin $ unwords glyphs

-- One iteration of the "fxhash" hash function, with state `a` and input `x`.
-- Based on the operations outlined in
-- https://nnethercote.github.io/2021/12/08/a-brutally-effective-hash-function-in-rust.html
fxHash :: Word64 -> Word64 -> Word64
fxHash !a !x = (xor (rotateL a 5) x) * 0x517cc1b727220a95

fxHashString :: String -> Word64
fxHashString = foldl' fxHash 0 . fmap (fromIntegral . fromEnum)

fxHashStrings :: [String] -> Word64
fxHashStrings = foldl' fxHash 0 . fmap fxHashString

-- Given a list of glyph names, return a short filename based on a hash of the
-- content, such that if the content changes, the name is different. Subsetted
-- fonts are stored "content-addressable". The file contents are immutable. The
-- advantage of this is that the content is stable, so if the file exists, it
-- does not need to be generated, and also that if the content does change, the
-- filename is different, which means there are no issues with cache
-- invalidation.
--
-- There is one question: how long to make the hash? It should be short, because
-- the filename will essentially be random. Including a few random file names in
-- my pages will increase the page size, so the hashes should not be longer than
-- necessary. On the other hand, they should be long enough to be practically
-- unique and avoid collisions. It's an instance of the birthday problem. I have
-- about 30 pages on my blog at the moment of writing. Let's say I will have at
-- most 200 pages ever. The collision probability can be computed as
--
--     1 - (1 - 1/2^bits) ^ choose(npages, 2)
--
-- Then I want the collision probability to be at most 10e-5 or so, so taking 32
-- bits works. 28 might work as well, but I'll take 32 to be safe.
contentName :: [String] -> String
contentName glyphNames =
  let
    hash32 :: Word32 = fromIntegral $ fxHashStrings glyphNames
    appendHexHash = showHex hash32
    name = appendHexHash ""
    prefix = take (8 - (length name)) (repeat '0') -- Left-pad with zeros.
  in
    prefix ++ name

-- Given font destination directory and html contents, generates subset commands
-- that will output subsetted fonts with the given basename and a content-based
-- suffix:
--
--  * "m" for monospace, subset of Inconsolata.
--  * "sr", "si", and "sb" for the roman, italic, and bold subset of Calluna.
--  * "r", "i", and "b" for their sans-serif variants, subset of Calluna Sans.
--
-- Both a woff and woff2 file will be written. Also returns a template context
-- that contains the font file basenames.
subsetArtifact :: FilePath -> String -> (Template.Context, [SubsetCommand])
subsetArtifact fontOutDir html = (fontContext, filter isUseful commands)
  where isUseful (SubsetCommand _ _ glyphs) = not $ null glyphs
        fontPieces = mapFontFull html

        serifItalicGlyphs = getGlyphs (Serif, Regular, Italic) WithDiscretionaryLigatures fontPieces
        serifRomanGlyphs  = getGlyphs (Serif, Regular, Roman)  WithLigatures fontPieces
        serifBoldGlyphs   = getGlyphs (Serif, Bold,    Roman)  WithLigatures fontPieces
        sansItalicGlyphs  = getGlyphs (Sans,  Regular, Italic) WithLigatures fontPieces
        sansRomanGlyphs   = getGlyphs (Sans,  Regular, Roman)  WithLigatures fontPieces
        sansBoldGlyphs    = getGlyphs (Sans,  Bold,    Roman)  WithLigatures fontPieces
        monoGlyphs        = getGlyphs (Mono,  Regular, Roman)  NoLigatures   fontPieces

        fontInDir = "fonts/generated/"
        subset file prefix glyphs =
          SubsetCommand (fontInDir ++ file) (fontOutDir ++ prefix ++ contentName glyphs) glyphs

        serifItalicCommand = subset "calluna-italic.otf"      "si" serifItalicGlyphs
        serifRomanCommand  = subset "calluna.otf"             "sr" serifRomanGlyphs
        serifBoldCommand   = subset "calluna-bold.otf"        "sb" serifBoldGlyphs
        sansItalicCommand  = subset "calluna-sans-italic.otf" "i"  sansItalicGlyphs
        sansRomanCommand   = subset "calluna-sans.otf"        "r"  sansRomanGlyphs
        sansBoldCommand    = subset "calluna-sans-bold.otf"   "b"  sansBoldGlyphs
        monoCommand        = subset "inconsolata.otf"         "m"  monoGlyphs

        fontContext = Map.unions
          [ Template.stringField "serif-italic-hash" $ contentName serifItalicGlyphs
          , Template.stringField "serif-roman-hash"  $ contentName serifRomanGlyphs
          , Template.stringField "serif-bold-hash"   $ contentName serifBoldGlyphs
          , Template.stringField "sans-italic-hash"  $ contentName sansItalicGlyphs
          , Template.stringField "sans-roman-hash"   $ contentName sansRomanGlyphs
          , Template.stringField "sans-bold-hash"    $ contentName sansBoldGlyphs
          , Template.stringField "mono-hash"         $ contentName monoGlyphs
          ]

        commands = [ serifItalicCommand, serifRomanCommand, serifBoldCommand
                   , sansItalicCommand,  sansRomanCommand,  sansBoldCommand
                   , monoCommand ]

-- Returns whether the html contains text that must be set in the bold
-- sans-serif font.
usesBoldFont :: String -> Bool
usesBoldFont = not . null . filter isBoldSans . mapFontFull
  where isBoldSans (_, (Sans, Bold, _, _)) = True
        isBoldSans _                       = False

-- Returns whether the html contains text that must be set in the serif italic
-- font.
usesSerifItalicFont :: String -> Bool
usesSerifItalicFont = not . null . filter isSerifItalic . mapFontFull
  where isSerifItalic (_, (Serif, _, Italic, _)) = True
        isSerifItalic _                          = False

-- Replaces double dashes (--) surrounded by spaces with em-dashes (—)
-- surrounded by spaces, and single dashes surrounded by spaces with
-- en-dashes surrounded by spaces. Also replaces triple dots with an
-- ellipsis (…).
expandPunctuationRaw :: String -> String
expandPunctuationRaw str = case str of
  -- The code point U+2014 is an em-dash (—), U+2013 an en-dash (–). Though
  -- they can be embedded in string literals directly, they are escaped because
  -- in an editor with a monospace font they can be difficult to distinguish.
  s1:'-':'-':s2:more -> if isSpace s1 && isSpace s2
                          then " \x2014 " ++ expandPunctuationRaw more
                          else s1 : '-' : '-' : s2 : expandPunctuationRaw more
  s1:'-':s2:more     -> if isSpace s1 && isSpace s2
                          then " \x2013 " ++ expandPunctuationRaw more
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

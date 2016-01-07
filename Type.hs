-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Type ( SubsetCommand
            , expandPunctuation
            , makeAbbrs
            , subsetArtifact
            , subsetFonts
            , usesBoldFont
            ) where

import           Data.Char (isAscii, isAsciiLower, isAsciiUpper, isLetter,
                            isSpace, ord, toLower, toUpper)
import           Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import           System.IO (hClose, hPutStrLn)
import qualified System.Process as P
import qualified Text.HTML.TagSoup as S

import qualified Html

--type Tag = Html.Tag
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
  _ | Html.isMath t   -> False
  _ | Html.isScript t -> False
  _ | Html.isStyle t  -> False
  _ | otherwise       -> True

getFamily :: TagProperties -> FontFamily
getFamily t = case t of
  _ | Html.isCode t       -> Mono
  _ | Html.isH3 t         -> Sans
  _ | Html.isHeader t     -> Serif
  _ | Html.isHeading t    -> Serif
  _ | Html.isTeaserLink t -> Serif
  _ | otherwise           -> Sans

getWeight :: TagProperties -> FontWeight
getWeight t = case t of
  _ | Html.isSubtitle t -> Regular
  _ | Html.isHeading t  -> Bold
  _ | Html.isStrong t   -> Bold
  _ | Html.isTh t       -> Bold
  _ | otherwise         -> Regular

getStyle :: TagProperties -> FontStyle
getStyle t = case t of
  _ | Html.isEm t       -> Italic
  _ | Html.isSubtitle t -> Italic
  _ | otherwise         -> Roman

getCaps :: TagProperties -> FontCaps
getCaps t = case t of
  _ | Html.isAbbr t       -> AllSmallCaps
  _ | Html.isSmcp t       -> AllSmallCaps
  _ | Html.isTeaserLink t -> AllSmallCaps
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
        -- but not in monospace content (code).
        isBodyTag t = (needsFont t) && (not $ Html.isCode t)

-- Returns whether Calluna or Inconsolata has a glyph for the character. This
-- function is optimistic, so getGlyphName still fails for unexpected glyphs.
isGlyphSupported :: Char -> Bool
isGlyphSupported c = not $ c `elem` ['\n', 'φ', 'ψ', '≡', '𝔽']

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
  '=' -> "equal"
  '>' -> "greater"
  '?' -> "question"
  '@' -> "at"
  '[' -> "bracketleft"
  ']' -> "bracketright"
  '_' -> "underscore"
  '`' -> "grave"
  '{' -> "braceleft"
  '|' -> "bar"
  '}' -> "braceright"
  '±' -> "plusminus"
  '·' -> "periodcentered"
  '»' -> "guillemotright" -- A typo in the postscript specification.
  'é' -> "eacute"
  'ë' -> "edieresis"
  '‘' -> "quoteleft"
  '’' -> "quoteright"
  '“' -> "quotedblleft"
  '”' -> "quotedblright"
  '•' -> "bullet"
  '…' -> "ellipsis"
  '≈' -> "approxequal"
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
            -- Calluna also has ip and it ligatures, but I find those too
            -- excessive for my subheadings.
            []             -> glyphs
            'c':'b':xs     -> liga xs "c_b"
            'c':'h':xs     -> liga xs "c_h"
            'c':'k':xs     -> liga xs "c_k"
            'c':'p':xs     -> liga xs "c_p"
            'c':'t':xs     -> liga xs "ct"
            'g':'i':xs     -> liga xs "g_i"
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

-- Given html, returns a string with guillemets that are added by css.
synthesizeGuillemets :: String -> String
synthesizeGuillemets html = if hasTeaserLink then "\x00a0»" else ""
  where hasTeaserLink  = not $ null
                       $ filter (isTeaserLink . snd)
                       $ Html.classifyTags
                       $ Html.parseTags html
        isTeaserLink t = (Html.isA t) && (Html.isTeaser t)

-- Returns the text that is not present in the html but generated by css.
synthesizeFont :: String -> [(String, FontAndCaps)]
synthesizeFont html =
  [ (synthesizeListNumbers html, (Sans,  Bold,    Roman, UnchangedCaps))
  , (synthesizeListBullets html, (Sans,  Regular, Roman, UnchangedCaps))
  , (synthesizeGuillemets  html, (Serif, Regular, Roman, AllSmallCaps )) ]

-- Returns all the text that should be typeset and the font it should be set in.
mapFontFull :: String -> [(String, FontAndCaps)]
mapFontFull html = (mapFont html) ++ (synthesizeFont html)

data IncludeLigatures = NoLigatures
                      | WithLigatures
                      | WithDiscretionaryLigatures

-- Given a font and wether to include ligatures, and content processed by
-- mapFont, returns a list of postscript glyph names required to typeset the
-- content.
getGlyphs :: Font -> IncludeLigatures -> [(String, FontAndCaps)] -> [String]
getGlyphs font ligatures = unique . (concatMap mapGlyphs) . (filter matchesFont)
        -- Note: there are more small cap glyphs than just the letters, but for
        -- now I don't use them.
  where makeSmcp glyph = case glyph of
          -- For a small cap, include both the small cap and the regular
          -- capital. Because of the way opentype works, the capital needs to be
          -- included even if it is not used, because small caps follow a
          -- substitution rule, and if the true capital is not there, there is
          -- nothing to substitute.
          g:[] | isAsciiLower g -> [g : ".smcp", [toUpper g]]
          _                     -> [glyph]
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
          SmallCaps     -> concatMap makeSmcp $ glyphsFor str
          AllSmallCaps  -> concatMap makeSmcp $ glyphsFor $ fmap toLower str

-- A subset command is the source font filename, the destination basename, and
-- the glyph names of the glyphs to subset.
data SubsetCommand = SubsetCommand FilePath FilePath [String] deriving (Show)

subsetFonts :: [SubsetCommand] -> IO ()
subsetFonts commands = do
  -- Divide the workload over eight processes to speed up subsetting.
  procs <- sequence $ replicate 8 $ P.createProcess subsetScriptPiped
  let stdins = fmap (\(Just stdin, _, _, _) -> stdin) procs
      pids   = fmap (\(_, _, _, pid) -> pid) procs
  mapM_ (uncurry pushCommand) (zip (cycle stdins) commands)
  mapM_ hClose stdins
  mapM_ P.waitForProcess pids -- Wait, but ignore the exit codes.
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
--  * "m" for monospace, subset of Inconsolata.
--  * "sr", "si", and "sb" for the roman, italic, and bold subset of Calluna.
--  * "r", "i", and "b" for their sans-serif variants, subset of Calluna Sans.
--
--  Both a woff and woff2 file will be written.
subsetArtifact :: FilePath -> String -> [SubsetCommand]
subsetArtifact baseName html = filter isUseful commands
  where isUseful (SubsetCommand _ _ glyphs) = not $ null glyphs
        subset file suffix glyphs = SubsetCommand file (baseName ++ suffix) glyphs
        fontPieces = mapFontFull html

        serifItalicGlyphs = getGlyphs (Serif, Regular, Italic) WithDiscretionaryLigatures fontPieces
        serifRomanGlyphs  = getGlyphs (Serif, Regular, Roman)  WithLigatures fontPieces
        serifBoldGlyphs   = getGlyphs (Serif, Bold,    Roman)  WithLigatures fontPieces
        sansItalicGlyphs  = getGlyphs (Sans,  Regular, Italic) WithLigatures fontPieces
        sansRomanGlyphs   = getGlyphs (Sans,  Regular, Roman)  WithLigatures fontPieces
        sansBoldGlyphs    = getGlyphs (Sans,  Bold,    Roman)  WithLigatures fontPieces
        monoGlyphs        = getGlyphs (Mono,  Regular, Roman)  NoLigatures   fontPieces

        serifItalicCommand = subset "fonts/calluna-italic.otf"      "si" serifItalicGlyphs
        serifRomanCommand  = subset "fonts/calluna.otf"             "sr" serifRomanGlyphs
        serifBoldCommand   = subset "fonts/calluna-bold.otf"        "sb" serifBoldGlyphs
        sansItalicCommand  = subset "fonts/calluna-sans-italic.otf" "i"  sansItalicGlyphs
        sansRomanCommand   = subset "fonts/calluna-sans.otf"        "r"  sansRomanGlyphs
        sansBoldCommand    = subset "fonts/calluna-sans-bold.otf"   "b"  sansBoldGlyphs
        monoCommand        = subset "fonts/inconsolata.otf"         "m"  monoGlyphs

        commands = [ serifItalicCommand, serifRomanCommand, serifBoldCommand
                   , sansItalicCommand,  sansRomanCommand,  sansBoldCommand
                   , monoCommand ]

-- Returns whether the html contains text that must be set in the bold
-- sans-serif font.
usesBoldFont :: String -> Bool
usesBoldFont = not . null . filter isBoldSans . mapFontFull
  where isBoldSans (_, (Sans, Bold, _, _)) = True
        isBoldSans _                       = False

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

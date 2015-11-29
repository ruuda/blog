-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Type ( SubsetCommand
            , expandPunctuation
            , getBoldText
            , getCode
            , getItalicText
            , makeAbbrs
            , subsetArtifact
            , subsetFonts
            ) where

import           Data.Char (isAscii, isAsciiUpper, isLetter, isSpace, ord, toLower)
import qualified Data.Set as Set
import           System.IO (hClose, hPutStrLn)
import qualified System.Process as P
import qualified Text.HTML.TagSoup as S

import qualified Html

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

-- Extracts all text between <code> tags.
getCode :: String -> String
getCode = Html.getTextInTag Html.isCode

-- Extracts all text between <em> tags.
getItalicText :: String -> String
getItalicText = Html.getTextInTag Html.isEm

-- Extracts all text between <strong> tags.
getBoldText :: String -> String
getBoldText = Html.getTextInTag Html.isStrong

-- Returns the post title and section headings.
getHeadingText :: String -> String
getHeadingText = Html.getTextInTag isHeading
  -- The heading font is used for the post heading (h1), and the section
  -- headings (h2). The post subheading (h2 in header) should not be included.
  where isHeading t = (Html.isH1 t) || ((Html.isH2 t) && (not $ Html.isHeader t))

-- Returns the subheading of a post (<h2> inside <header>).
getSubheadingText :: String -> String
getSubheadingText = Html.getTextInTag (\t -> (Html.isH2 t) && (Html.isHeader t))

-- Returns the text that should be set in serif (<p> inside <header>).
getSerifText :: String -> String
getSerifText = Html.getTextInTag (\t -> (Html.isHeader t) &&
                                        (not $ Html.isH1 t) &&
                                        (not $ Html.isH2 t))

-- Returns the text that should be typeset in sans-serif small caps.
getSmallCapsText :: String -> String
getSmallCapsText = fmap toLower .
                   filter (not . isSpace) .
                   Html.getTextInTag Html.isAbbr

isBodyTag :: Html.TagProperties -> Bool
isBodyTag tag = (not $ Html.isCode   tag) && -- <code> uses monospace font.
                (not $ Html.isEm     tag) && -- <em> uses italic font.
                (not $ Html.isH1     tag) && -- <h1> uses bold serif font.
                (not $ Html.isH2     tag) && -- <h2> uses bold serif font.
                (not $ Html.isHead   tag) && -- Exclude non-body tags.
                (not $ Html.isHeader tag) && -- <header> uses serif font.
                (not $ Html.isMath   tag) &&
                (not $ Html.isScript tag) &&
                (not $ Html.isStrong tag) && -- <strong> uses bold font.
                (not $ Html.isStyle  tag)

-- Returns the text that should be typeset with the body font. Mostly this is
-- everything that should not be typeset with a different font.
getBodyText :: String -> String
getBodyText = Html.getTextInTag isBodyTag

-- Splits a string into words at spaces, dashes and dots. Does not discard any
-- characters, split points become single-character elements.
splitWords :: String -> [String]
splitWords = filter (not . null) . foldr prepend [""]
  where prepend _ []          = error "unreachable"
        prepend c (word:more) = if not $ isLetter c
                                then "" : [c] : word : more
                                else (c : word) : more

data AbbrWord = Regular String | AllCaps String

abbrNull :: AbbrWord -> Bool
abbrNull (Regular str) = null str
abbrNull (AllCaps str) = null str

-- Splits a string into sentences and all-caps words alternatingly.
splitAbbrs :: String -> [AbbrWord]
splitAbbrs = filter (not . abbrNull) . foldr prepend [Regular ""] . splitWords
  where prepend _    []              = error "unreachable"
        prepend word (AllCaps str : more) = Regular word : AllCaps str : more
        prepend word (Regular str : more) = if (all isAsciiUpper word) && (length word >= 2)
                                            then AllCaps word : Regular str : more
                                            else Regular (word ++ str) : more

-- Inserts <abbr> tags around words in all-caps.
-- TODO: Due to my definition of isBodyTag, this will not add <abbr>s inside
-- <em> or <strong> or <h1> or <h2>. That should be fixed.
makeAbbrs :: String -> String
makeAbbrs = Html.renderTags . Html.concatMapTagsWhere isBodyTag mkAbbr . Html.parseTags
  where mkAbbr (S.TagText str)    = concatMap insertAbbrs $ splitAbbrs str
        mkAbbr tag                = [tag]
        insertAbbrs (Regular str) = [S.TagText str]
        insertAbbrs (AllCaps str) = [S.TagOpen "abbr" [], S.TagText str, S.TagClose "abbr"]

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
  'é' -> "eacute"
  'ë' -> "edieresis"
  '‘' -> "quoteleft"
  '’' -> "quoteright"
  '“' -> "quotedblleft"
  '”' -> "quotedblright"
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
    dligGlyphs = unique $ getDiscretionaryLigatures str

-- Returns a list of postscript glyph names required to typeset the content of
-- all <code> tags in the string.
getCodeGlyphs :: String -> [String]
getCodeGlyphs = getGlyphs NoLigatures . getCode

-- Returns a list of postscript glyph names required to typeset the content of
-- all italic body text in a post. (The text between <em> tags.)
getItalicGlyphs :: String -> [String]
getItalicGlyphs = getGlyphs WithLigatures . getItalicText

-- Returns a list of postscript glyph names required to typeset the content of
-- all bold body text in a post. (The text between <strong> tags.)
getBoldGlyphs :: String -> [String]
getBoldGlyphs = getGlyphs WithLigatures . getBoldText

-- Returns a list of postscript glyph names required to typeset the titles and
-- section headings in a post.
getHeadingGlyphs :: String -> [String]
getHeadingGlyphs = getGlyphs WithLigatures . getHeadingText

-- Returns a list of postscript glyph names required to typeset the post
-- subheading.
getSubheadingGlyphs :: String -> [String]
getSubheadingGlyphs = getGlyphs WithDiscretionaryLigatures . getSubheadingText

-- Returns a list of postscript glyph names required to typeset the serif text.
getSerifGlyphs :: String -> [String]
getSerifGlyphs = getGlyphs WithLigatures . getSerifText

-- Returns a list of postscript glyph names required to typeset the body text.
getBodyGlyphs :: String -> [String]
getBodyGlyphs = getGlyphs WithLigatures . getBodyText

getSmallCapsGlyphs :: String -> [String]
getSmallCapsGlyphs = getGlyphs NoLigatures . getSmallCapsText

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
        subset file suffix glyphs = SubsetCommand file (baseName ++ suffix) glyphs
        -- The names of small caps glyphs are equal to the normal names, but
        -- with a ".smcp" suffix.
        smallCapsGlyphs   = fmap (++ ".smcp") $ getSmallCapsGlyphs html
        bodyGlyphs        = (getBodyGlyphs html) ++ smallCapsGlyphs
        boldGlyphs        = getBoldGlyphs html
        headingGlyphs     = getHeadingGlyphs html
        serifGlyphs       = getSerifGlyphs html
        subheadingGlyphs  = getSubheadingGlyphs html
        italicGlyphs      = getItalicGlyphs html
        monoGlyphs        = getCodeGlyphs html
        bodyCommand       = subset "fonts/calluna-sans.otf" "r" bodyGlyphs
        boldCommand       = subset "fonts/calluna-sans-bold.otf" "b" boldGlyphs
        headingCommand    = subset "fonts/calluna-bold.otf" "bs" headingGlyphs
        serifCommand      = subset "fonts/calluna.otf" "s" serifGlyphs
        subheadingCommand = subset "fonts/calluna-italic.otf" "is" subheadingGlyphs
        italicCommand     = subset "fonts/calluna-sans-italic.otf" "i" italicGlyphs
        monoCommand       = subset "fonts/inconsolata.otf" "m" monoGlyphs
        commands          = [ bodyCommand
                            , boldCommand
                            , headingCommand
                            , serifCommand
                            , subheadingCommand
                            , italicCommand
                            , monoCommand ]

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

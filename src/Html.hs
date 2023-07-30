-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Html ( Tag
            , TagProperties
            , makeRunIn
            , addAnchors
            , applyTagsWhere
            , classifyTags
            , cleanTables
            , cleanOl
            , cleanCodeBlocks
            , concatMapTagsWhere
            , filterTags
            , getTextInTag
            , hasUl
            , hasH2
            , hasMath
            , hasImg
            , isA
            , isAbbr
            , isArchive
            , isArchiveLink
            , isArticle
            , isBlockQuote
            , isCode
            , isEm
            , isH1
            , isH2
            , isH3
            , isHead
            , isHeader
            , isHeading
            , isOl
            , isPre
            , isRunIn
            , isScript
            , isSmcp
            , isSubtitle
            , isSub
            , isSup
            , isStrong
            , isStyle
            , isTable
            , isTeaser
            , isTeaserLink
            , isTh
            , isUl
            , isVar
            , mapTagsWhere
            , mapText
            , mapTextWith
            , maxOlLength
            , mergeStyles
            , parseTags
            , renderTags
            ) where

-- This module contains utility functions for dealing with html.

import           Control.Monad (join, msum)
import           Data.List (delete, intersperse)
import           Data.Maybe (catMaybes)
import qualified Text.HTML.TagSoup as S

type Tag = S.Tag String

-- Tagsoup's default escape function escapes " to &quot;, but this is only
-- required inside quoted strings and only bloats the html in other places.
-- Even worse, it can render inline stylesheets invalid. I do not have
-- quoted strings with quotes in them, so it is fine not to escape quotes.
escapeHtml :: String -> String
escapeHtml = concatMap escape
  where escape '&' = "&amp;"
        escape '<' = "&lt;"
        escape '>' = "&gt;"
        escape  c  = [c]

-- Render options for Tagsoup that use the above escape function, and and that
-- do not escape inside <style> tags in addition to the default <script> tags.
renderOptions :: S.RenderOptions String
renderOptions = S.RenderOptions escapeHtml minimize rawTag
  where minimize _ = False -- Do not omit closing tags for empty tags.
        rawTag tag = (tag == "script") || (tag == "style")

-- Like Tagsoup's renderTags, but with the above options applied.
renderTags :: [Tag] -> String
renderTags = S.renderTagsOptions renderOptions

-- Reexport of Tagsoup's parseTags for symmetry.
parseTags :: String -> [Tag]
parseTags = S.parseTags

-- Applies a function to the text of a text tag.
mapText :: (String -> String) -> Tag -> Tag
mapText f (S.TagText str) = S.TagText (f str)
mapText _ tag             = tag

-- Various classifications for tags: inside body, inside code, etc.
data TagClass = A
              | Abbr
              | Archive -- Not an html tag, but an id.
              | Article
              | BlockQuote
              | Code
              | Em
              | H1
              | H2
              | H3
              | Head
              | Header
              | Ol
              | Pre
              | RunIn  -- Not an html tag, but a class.
              | Script
              | Smcp   -- Not an html tag, but a class.
              | Style
              | Strong
              | Sub
              | Sup
              | Table
              | Teaser -- Not an html tag, but an id.
              | Th
              | Ul
              | Var
              deriving (Eq, Ord, Show)

tagClassFromName :: String -> Maybe TagClass
tagClassFromName name = case name of
  "a"          -> Just A
  "abbr"       -> Just Abbr
  "article"    -> Just Article
  "blockquote" -> Just BlockQuote
  "code"       -> Just Code
  "em"         -> Just Em
  "h1"         -> Just H1
  "h2"         -> Just H2
  "h3"         -> Just H3
  "head"       -> Just Head
  "header"     -> Just Header
  "ol"         -> Just Ol
  "pre"        -> Just Pre
  "script"     -> Just Script
  "style"      -> Just Style
  "strong"     -> Just Strong
  "sub"        -> Just Sub
  "sup"        -> Just Sup
  "table"      -> Just Table
  "th"         -> Just Th
  "ul"         -> Just Ul
  "var"        -> Just Var
  _            -> Nothing

tagClassFromAttributes :: [(String, String)] -> Maybe TagClass
tagClassFromAttributes = msum . fmap fromAttr
  where fromAttr attr = case attr of
          ("class", "smcp")    -> Just Smcp
          ("class", "run-in")  -> Just RunIn
          ("class", "archive") -> Just Archive
          ("id", "teaser")     -> Just Teaser
          _                    -> Nothing

-- Try to classify the tag based on the tag name and based on the attributes.
tagClass :: String -> [(String, String)] -> [TagClass]
tagClass name attrs = catMaybes [tagClassFromName name, tagClassFromAttributes attrs]

-- A stack of tag name (string) and classification.
type TagStack = [(String, [TagClass])]

updateTagStack :: TagStack -> Tag -> TagStack
updateTagStack ts tag = case tag of
  S.TagOpen name attrs -> case tagClass name attrs of
   []             -> ts
   classification -> (name, classification) : ts
  S.TagClose name -> case ts of
    (topName, _) : more -> if topName == name then more else ts
    _                   -> ts
  _                     -> ts

-- Determines for every tag the nested tag classifications.
tagStacks :: [Tag] -> [[TagClass]]
tagStacks = fmap (concatMap snd) . scanl updateTagStack []

data TagProperties = TagProperties { isA          :: Bool
                                   , isAbbr       :: Bool
                                   , isArchive    :: Bool
                                   , isArticle    :: Bool
                                   , isBlockQuote :: Bool
                                   , isCode       :: Bool
                                   , isEm         :: Bool
                                   , isH1         :: Bool
                                   , isH2         :: Bool
                                   , isH3         :: Bool
                                   , isHead       :: Bool
                                   , isHeader     :: Bool
                                   , isOl         :: Bool
                                   , isPre        :: Bool
                                   , isRunIn      :: Bool
                                   , isScript     :: Bool
                                   , isSmcp       :: Bool
                                   , isStyle      :: Bool
                                   , isStrong     :: Bool
                                   , isSub        :: Bool
                                   , isSup        :: Bool
                                   , isTable      :: Bool
                                   , isTeaser     :: Bool
                                   , isTh         :: Bool
                                   , isUl         :: Bool
                                   , isVar        :: Bool }

isHeading :: TagProperties -> Bool
isHeading t = (isH1 t) || (isH2 t) || (isH3 t)

isSubtitle :: TagProperties -> Bool
isSubtitle t = (isHeader t) && (isH2 t)

isArchiveLink :: TagProperties -> Bool
isArchiveLink t = (isArchive t) && (isSmcp t) && (isA t)

isTeaserLink :: TagProperties -> Bool
isTeaserLink t = (isTeaser t) && (isA t)

getProperties :: [TagClass] -> TagProperties
getProperties ts =
  let test cls = (cls `elem` ts)
  in TagProperties { isA          = test A
                   , isAbbr       = test Abbr
                   , isArchive    = test Archive
                   , isArticle    = test Article
                   , isBlockQuote = test BlockQuote
                   , isCode       = test Code
                   , isEm         = test Em
                   , isH1         = test H1
                   , isH2         = test H2
                   , isH3         = test H3
                   , isHead       = test Head
                   , isHeader     = test Header
                   , isOl         = test Ol
                   , isPre        = test Pre
                   , isRunIn      = test RunIn
                   , isScript     = test Script
                   , isSmcp       = test Smcp
                   , isStyle      = test Style
                   , isStrong     = test Strong
                   , isSub        = test Sub
                   , isSup        = test Sup
                   , isTable      = test Table
                   , isTeaser     = test Teaser
                   , isTh         = test Th
                   , isUl         = test Ul
                   , isVar        = test Var }

-- Given a list of tags, classifies them as "inside code", "inside em", etc.
classifyTags :: [Tag] -> [(Tag, TagProperties)]
classifyTags tags = zip tags $ fmap getProperties $ tagStacks tags

-- Discards tags for which the predicate returns false.
filterTags :: (TagProperties -> Bool) -> [Tag] -> [Tag]
filterTags predicate = fmap fst . filter (predicate . snd) . classifyTags

-- Applies a mapping function to the tags when the predicate p returns true for
-- that tag. The function tmap is a way to abstract over the mapping function,
-- it should not alter the length of the list.
applyTagsWhere :: (TagProperties -> Bool) -> ([Tag] -> [Tag]) -> [Tag] -> [Tag]
applyTagsWhere p tmap tags = fmap select $ zip (classifyTags tags) (tmap tags)
  where select ((orig, props), mapped) = if p props then mapped else orig

-- Applies the function f to all tags for which p returns true.
mapTagsWhere :: (TagProperties -> Bool) -> (Tag -> Tag) -> [Tag] -> [Tag]
mapTagsWhere p f = applyTagsWhere p (fmap f)

-- Applies the function f to all tags for which p returns true and flattens the result.
concatMapTagsWhere :: (TagProperties -> Bool) -> (Tag -> [Tag]) -> [Tag] -> [Tag]
concatMapTagsWhere p f = concatMap select . classifyTags
  where select (tag, props) = if (p props) then f tag else [tag]

-- Returns the the text in all tags that satisfy the selector.
getTextInTag :: (TagProperties -> Bool) -> String -> String
getTextInTag p  = join . intersperse " " . getText . (filterTags p) . parseTags
  where getText = fmap S.fromTagText . filter S.isTagText

-- Returns a list of text in text nodes, together with a value selected by f.
mapTextWith :: (TagProperties -> a) -> String -> [(String, a)]
mapTextWith f = fmap select . (filter $ S.isTagText . fst) . classifyTags . parseTags
  where select (tag, props) = (S.fromTagText tag, f props)

-- Returns whether an <ul> tag is present in the <article> in an html string.
hasUl :: String -> Bool
hasUl = not . null
      . filter (isArticle . snd)
      . filter (S.isTagOpenName "ul" . fst)
      . classifyTags
      . parseTags

-- Returns whether a <h2> tag is present in the <article> in an html string.
hasH2 :: String -> Bool
hasH2 = not . null
      . filter (isArticle . snd)
      . filter (S.isTagOpenName "h2" . fst)
      . classifyTags
      . parseTags

-- Returns whether an html snippet contains a <sub>, <sup>, or <var> tag.
hasMath :: String -> Bool
hasMath = any (\t -> isSub t || isSup t || isVar t)
        . fmap snd
        . classifyTags
        . parseTags

-- Returns whether a <img> tag is present in the html string. Note that although
-- we later replace <img> with <object> for svg images, at the time when this
-- scan runs, we still have the <img>.
hasImg :: String -> Bool
hasImg = not . null
       . filter (S.isTagOpenName "img")
       . parseTags

-- Returns the length of the longest ordered list in an html string.
maxOlLength :: String -> Int
maxOlLength = maximum . foldl listLength [0] . classifyTags . parseTags
  where listLength ns     ((S.TagOpen  "ol" _), _  )            = 0 : ns
        listLength (n:ns) ((S.TagOpen  "li" _), cls) | isOl cls = (n + 1) : ns
        listLength ns     _                                     = ns

-- Adds <span class="run-in"> around the first n characters of an html snippet.
-- Assumes that the html starts with a <p> tag.
makeRunIn :: String -> Int -> String
makeRunIn html n  = prefix ++ (drop 3 runIn) ++ "</span>" ++ after
  where (runIn, after) = splitAt (n + 3) html
        prefix         = "<p><span class=\"run-in\">"

-- Given a piece of html, strips the "align" attributes from table elements.
-- Pandoc adds these alignment tags to tables, but they are deprecated in html5.
-- If you tell Pandoc to write html5, it will just add style attributes instead.
-- I always align left anyway, so strip the attribute altogether. Furthermore,
-- I do not use the odd and even classes, so strip them to save space.
cleanTables :: String -> String
cleanTables = renderTags . mapTagsWhere isTable stripAttrs . parseTags
  where filterAlign = filter $ (/= "align") . fst
        filterEven  = delete ("class", "even")
        filterOdd   = delete ("class", "odd")
        filterAttrs = filterAlign . filterEven . filterOdd
        stripAttrs tag = case tag of
          S.TagOpen name attrs -> S.TagOpen name $ filterAttrs attrs
          _                    -> tag

-- Pandoc started adding 'type="1"' or 'style="..." attributes to <ol> tags.
-- This is not needed, because I style them with css anyway.
cleanOl :: String -> String
cleanOl = renderTags . fmap cleanOlImpl . parseTags
  where
    cleanOlImpl tag = case tag of
      S.TagOpen "ol" _ -> S.TagOpen "ol" []
      somethingElse    -> somethingElse

-- Pandoc inserts anchors for every line in a code block. These take up a lot of
-- space because they look like
--
--   <a href="cb1-1" aria-hidden="true" tabindex="-1"></a>
--
-- And they add zero value in my case; they are not displayed, and the ids to
-- link to are present on the enclosing span either way, so you can still link
-- to lines if you like. So delete these <a> tags inside code blocks.
cleanCodeBlocks :: String -> String
cleanCodeBlocks = renderTags . concatMapTagsWhere isCode stripAnchor . parseTags
  where
    stripAnchor tag = case tag of
      S.TagOpen "a" _ -> []
      S.TagClose "a"  -> []
      somethingElse   -> [somethingElse]

-- Add an empty <a> tag to every <h2> that has an id, and link it to that id.
addAnchors :: String -> String
addAnchors = renderTags . concatMap expandHeader . parseTags
  where
    emptyA href = [S.TagOpen "a" [("href", href)], S.TagClose "a"]
    expandHeader tag = case tag of
      h2@(S.TagOpen "h2" [("id", anchor)]) -> h2 : emptyA ('#' : anchor)
      otherTag -> [otherTag]

-- Merge all <style> tags in the document into a single one. This helps with
-- minification, but mostly it helps to make the documents valid html, because
-- technically a <style> tag in the body is not valid, but for some posts I want
-- to write custom stylesheets for that post. So I just write them in the post,
-- Pandoc preserves them, and this function merges all style tags into the first
-- one, which is the one in the <head>.
mergeStyles :: [Tag] -> [Tag]
mergeStyles = run [] "" []
  where
    run pfx contents sfx [] =
      -- End of input, put everything back together. Note that the intermediate
      -- prefix and suffix lists were built reversed, so we reverse them again.
      (reverse pfx) <>
        [ S.TagOpen "style" []
        , S.TagText contents
        , S.TagClose "style"
        ] <>
        (reverse sfx)

    -- When we encounter a style tag, append its contents.
    run pfx contents1 sfx
      ( (S.TagOpen "style" _)
      : (S.TagText contents2)
      : (S.TagClose "style")
      : more
      ) = run pfx (contents1 <> contents2) sfx more

    -- Before we have style contents, every tag we are not interested in goes
    -- into the prefix. After we have style contents, everything goes into the
    -- suffix.
    run pfx ""       []  (tag:more) = run (tag:pfx) ""       []        more
    run pfx contents sfx (tag:more) = run pfx       contents (tag:sfx) more

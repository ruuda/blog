-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Html ( Tag
            , TagProperties
            , applyTagsWhere
            , classifyTags
            , filterTags
            , getTextInTag
            , isCode
            , isEm
            , isH1
            , isH2
            , isHead
            , isHeader
            , isMath
            , isPre
            , isScript
            , isStrong
            , isStyle
            , mapTagsWhere
            , mapText
            , parseTags
            , renderTags
            ) where

-- This module contains utility functions for dealing with html.

import           Control.Monad (join)
import           Data.List (intersperse)
import qualified Data.Map as M
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
data TagClass = Code
              | Em
              | H1
              | H2
              | Head
              | Header
              | Math
              | Pre
              | Script
              | Style
              | Strong
              | Other deriving (Eq, Ord)

tagClassFromString :: String -> TagClass
tagClassFromString str = case str of
  "code"   -> Code
  "em"     -> Em
  "h1"     -> H1
  "h2"     -> H2
  "head"   -> Head
  "header" -> Header
  "math"   -> Math
  "pre"    -> Pre
  "script" -> Script
  "style"  -> Style
  "strong" -> Strong
  _        -> Other

-- Used to count how much unclosed opening tags were encountered.
type TagDepth = M.Map TagClass Int

zeroDepth :: TagDepth
zeroDepth = M.fromList [ (Code,   0)
                       , (Em,     0)
                       , (H1,     0)
                       , (H2,     0)
                       , (Head,   0)
                       , (Header, 0)
                       , (Math,   0)
                       , (Pre,    0)
                       , (Script, 0)
                       , (Style,  0)
                       , (Strong, 0)
                       , (Other,  0) ]

updateTagDepth :: TagDepth -> Tag -> TagDepth
updateTagDepth td tag = case tag of
  S.TagOpen  name _ -> M.adjust (\d -> d + 1) (tagClassFromString name) td
  S.TagClose name   -> M.adjust (\d -> d - 1) (tagClassFromString name) td
  _                 -> td

-- Determines for every tag the nesting level of tag classifications.
tagDepths :: [Tag] -> [TagDepth]
tagDepths = scanl updateTagDepth zeroDepth

data TagProperties = TagProperties { isCode   :: Bool
                                   , isEm     :: Bool
                                   , isH1     :: Bool
                                   , isH2     :: Bool
                                   , isHead   :: Bool
                                   , isHeader :: Bool
                                   , isMath   :: Bool
                                   , isPre    :: Bool
                                   , isScript :: Bool
                                   , isStyle  :: Bool
                                   , isStrong :: Bool }

getProperties :: TagDepth -> TagProperties
getProperties td = TagProperties { isCode   = (td M.! Code)   > 0
                                 , isEm     = (td M.! Em)     > 0
                                 , isH1     = (td M.! H1)     > 0
                                 , isH2     = (td M.! H2)     > 0
                                 , isHead   = (td M.! Head)   > 0
                                 , isHeader = (td M.! Header) > 0
                                 , isMath   = (td M.! Math)   > 0
                                 , isPre    = (td M.! Pre)    > 0
                                 , isScript = (td M.! Script) > 0
                                 , isStyle  = (td M.! Style)  > 0
                                 , isStrong = (td M.! Strong) > 0 }

-- Given a list of tags, classifies them as "inside code", "inside em", etc.
classifyTags :: [Tag] -> [(Tag, TagProperties)]
classifyTags tags = zip tags $ fmap getProperties $ tagDepths tags

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

-- Returns the the text in all tags that satisfy the selector.
getTextInTag :: (TagProperties -> Bool) -> String -> String
getTextInTag p  = join . intersperse " " . getText . (filterTags p) . parseTags
  where getText = fmap S.fromTagText . filter S.isTagText

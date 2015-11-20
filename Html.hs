-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Html ( Tag
            , classifyTags
            , filterTags
            , getCode
            , getEmText
            , getStrongText
            , insideTag
            , isCode
            , isEm
            , isPre
            , isScript
            , isStrong
            , isStyle
            , mapTagsWhere
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
  where minimize tag = False -- Do not omit closing tags for empty tags.
        rawTag   tag = (tag == "script") || (tag == "style")

-- Like Tagsoup's renderTags, but with the above options applied.
renderTags :: [Tag] -> String
renderTags = S.renderTagsOptions renderOptions

-- Various classifications for tags: inside body, inside code, etc.
data TagClass = Code
              | Em
              | Pre
              | Script
              | Style
              | Strong
              | Other deriving (Eq, Ord)

tagClassFromString :: String -> TagClass
tagClassFromString str = case str of
  "code"   -> Code
  "em"     -> Em
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
                                   , isPre    :: Bool
                                   , isScript :: Bool
                                   , isStyle  :: Bool
                                   , isStrong :: Bool }

getProperties :: TagDepth -> TagProperties
getProperties td = TagProperties { isCode   = (td M.! Code)   > 0
                                 , isEm     = (td M.! Em)     > 0
                                 , isPre    = (td M.! Pre)    > 0
                                 , isScript = (td M.! Script) > 0
                                 , isStyle  = (td M.! Style)  > 0
                                 , isStrong = (td M.! Strong) > 0 }

-- Given a list of tags, classifies them as "inside code", "inside em", etc.
classifyTags :: [Tag] -> [(Tag, TagProperties)]
classifyTags tags = zip tags $ fmap getProperties $ tagDepths tags

-- Given a set of tag names and a list of tags, produces a list where the
-- elements are the current number of unclosed tags from the set.
insideTag :: [String] -> [Tag] -> [Int]
insideTag tagNames tags = scanl nestCount 0 tags
  where nestCount n (S.TagOpen name _) | name `elem` tagNames = n + 1
        nestCount n (S.TagClose name)  | name `elem` tagNames = n - 1
        nestCount n _ = n

-- Discards tags for which the predicate returns false.
filterTags :: (TagProperties -> Bool) -> [Tag] -> [Tag]
filterTags predicate = fmap fst . filter (predicate . snd) . classifyTags

-- Applies the function f to all tags for which p returns true.
mapTagsWhere :: (TagProperties -> Bool) -> (Tag -> Tag) -> [Tag] -> [Tag]
mapTagsWhere p f = fmap select . classifyTags
  where select (tag, properties) = if p properties then f tag else tag

-- Returns the the text in all tags that satisfy the selector.
getTextInTag :: (TagProperties -> Bool) -> String -> String
getTextInTag p  = join . intersperse " " . getText . (filterTags p) . S.parseTags
  where getText = fmap S.fromTagText . filter S.isTagText

-- Extracts all text between <code> tags.
getCode :: String -> String
getCode = getTextInTag isCode

-- Extracts all text between <em> tags.
getEmText :: String -> String
getEmText = getTextInTag isEm

-- Extracts all text between <strong> tags.
getStrongText :: String -> String
getStrongText = getTextInTag isStrong

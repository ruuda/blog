-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Html (Tag, insideTag, renderTags) where

-- This module contains utility functions for dealing with html.

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
  where minimize tag = False -- Do not omit closin tags for empty tags.
        rawTag   tag = (tag == "script") || (tag == "style")

-- Like Tagsoup's renderTags, but with the above options applied.
renderTags :: [Tag] -> String
renderTags = S.renderTagsOptions renderOptions

-- Given a set of tag names and a list of tags, produces a list where the
-- elements are the current number of unclosed tags from the set.
insideTag :: [String] -> [Tag] -> [Int]
insideTag tagNames tags = scanl nestCount 0 tags
  where nestCount n (S.TagOpen name _) | name `elem` tagNames = n + 1
        nestCount n (S.TagClose name)  | name `elem` tagNames = n - 1
        nestCount n _ = n

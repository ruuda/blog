-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Font (getEmText, getStrongText) where

import           Control.Monad (join)
import           Data.List (intersperse)
import qualified Text.HTML.TagSoup as S

import           Html (Tag, insideTag)

-- Returns the the text in all tags with the specified name.
getTextInTag :: String -> String -> String
getTextInTag name = join . intersperse " " . getText . filterInside . S.parseTags
  where inside       tags = fmap (> 0) $ insideTag [name] tags
        filterInside tags = fmap fst $ filter snd $ zip tags (inside tags)
        getText           = fmap S.fromTagText . filter S.isTagText

-- Extracts all text between <em> tags.
getEmText :: String -> String
getEmText = getTextInTag "em"

-- Extracts all text between <strong> tags.
getStrongText :: String -> String
getStrongText = getTextInTag "strong"

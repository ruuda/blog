-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Html (Tag, insideTag) where

-- This module contains utility functions for dealing with html.

import qualified Text.HTML.TagSoup as S

type Tag = S.Tag String

-- Given a set of tag names and a list of tags, produces a list where the
-- elements are the current number of unclosed tags from the set.
insideTag :: [String] -> [Tag] -> [Int]
insideTag tagNames tags = scanl nestCount 0 tags
  where nestCount n (S.TagOpen name _) | name `elem` tagNames = n + 1
        nestCount n (S.TagClose name)  | name `elem` tagNames = n - 1
        nestCount n _ = n

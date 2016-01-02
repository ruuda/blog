-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Image (processImages) where

import           Control.Monad ((>=>), liftM)

import qualified Html

-- Sets the width and height attributes of all <img> tags.
addDimensions :: FilePath -> [Html.Tag] -> IO [Html.Tag]
addDimensions imgDir html = error "not implemented"

addPlaceholders :: FilePath -> [Html.Tag] -> IO [Html.Tag]
addPlaceholders placeholderDir html = error "not implemented"

-- Given a piece of html, inserts a placeholder background for every <img> tag,
-- and adds the image dimensions to the attributes of the tag.
processImages :: FilePath -> FilePath -> String -> IO String
processImages imgDir placeholderDir = (liftM Html.renderTags)
  . (addDimensions imgDir >=> addPlaceholders placeholderDir)
  . Html.parseTags

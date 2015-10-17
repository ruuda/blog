-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import qualified Data.Map as M
import           Text.Pandoc

-- Front matter consists of key value pairs, both of type string.
-- There is no fancy YAML here.
type FrontMatter = M.Map String String

-- Strips off and parses front matter from the string. Front matter is
-- delimited by triple dashes. Keys are anything before ": ", the value
-- is what comes after that. Ignores first line assuming it is "---".
extractFrontMatter :: String -> (FrontMatter, String)
extractFrontMatter = parseFM M.empty . drop 1 . lines
  where parseFM fm ("---":body) = (fm, unlines body)
        parseFM fm (line:more)  = parseFM (M.insert key value fm) more
          where (key, delimValue) = break (== ':') line
                value = drop 2 delimValue -- Drop the colon and space.

-- A post is its front matter, plus the "body" field set to rendered html.
type Post = M.Map String String

renderPost :: String -> Post
renderPost str = M.insert "body" html fm
  where (fm, bodymd) = extractFrontMatter str
        ropt         = def -- TODO: set correct reader options
        wopt         = def -- TODO: set correct writer options
        html         = case fmap (writeHtmlString wopt) (readCommonMark ropt bodymd) of
          Right result -> result
          Left _       -> "failed to parse markdown"

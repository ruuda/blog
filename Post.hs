-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Post ( Post
            , body
            , date
            , longDate
            , renderPost
            , shortDate
            , slug
            , title
            , url
            , year ) where

import qualified Data.Map as M
import           Data.Time.Format
import           Data.Time.Calendar (Day, showGregorian, toGregorian)
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

data Post = Post { title :: String
                 , date  :: Day
                 , slug  :: String
                 , body  :: String } deriving (Show) -- TODO: This is for debugging only, remove.

-- Returns the post date, formatted like "17 April, 2015".
longDate :: Post -> String
longDate = formatTime defaultTimeLocale "%e %B, %Y" . date

-- Returns the post date, formatted like "2015-04-17".
shortDate :: Post -> String
shortDate = showGregorian . date

-- Returns the year in which the post was published.
year :: Post -> Integer
year post = y where (y, m, d) = toGregorian $ date post

-- Returns the canonical absolute url for a particular post.
url :: Post -> String
url post = "/" ++ datePath ++ "/" ++ (slug post)
  where datePath = formatTime defaultTimeLocale "%Y/%m/%d" $ date post

-- Given a slug and the contents of the post file (markdown with front matter),
-- renders the body to html and parses the metadata.
renderPost :: String -> String -> Post
renderPost slug contents = Post {
  title = frontMatter M.! "title",
  date  = parseTimeOrError True defaultTimeLocale "%F" (frontMatter M.! "date"),
  slug  = slug,
  body  = renderMarkdown bodyContents
} where (frontMatter, bodyContents) = extractFrontMatter contents

-- Renders markdown to html using Pandoc with my settings.
renderMarkdown :: String -> String
renderMarkdown md = case fmap (writeHtmlString wopt) (readCommonMark ropt md) of
  Right result -> result
  Left  _      -> "Failed to parse markdown."
  where ropt = def -- TODO: set correct reader options.
        wopt = def -- TODO: set correct writer options.

-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Post ( Post
            , archiveContext
            , body
            , context
            , date
            , longDate
            , parse
            , relatedContext
            , shortDate
            , selectRelated
            , slug
            , title
            , url
            , year ) where

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as Text
import           Data.Time.Format
import           Data.Time.Calendar (Day, showGregorian, toGregorian)
import           GHC.Exts (groupWith, sortWith)
import           Text.Pandoc

import qualified Html
import qualified Template as T
import qualified Type

-- Front matter consists of key value pairs, both of type string.
-- There is no fancy YAML here.
type FrontMatter = M.Map String String

-- Strips off and parses front matter from the string. Front matter is
-- delimited by triple dashes. Keys are anything before ": ", the value
-- is what comes after that. Ignores first line assuming it is "---".
extractFrontMatter :: String -> (FrontMatter, String)
extractFrontMatter = parseFM M.empty . drop 1 . lines
  where parseFM _ []                = error "Post should not be empty."
        parseFM fm ("---":postBody) = (fm, unlines postBody)
        parseFM fm (line:more)      = parseFM (M.insert key value fm) more
          where (key, delimValue) = break (== ':') line
                value = drop 2 delimValue -- Drop the colon and space.

data Post = Post { title     :: String
                 , header    :: String
                 , subheader :: Maybe String
                 , part      :: Maybe Int
                 , date      :: Day
                 , slug      :: String
                 , synopsis  :: String
                 , body      :: String } deriving (Show) -- TODO: This is for debugging only, remove.

-- Returns the post date, formatted like "17 April, 2015".
longDate :: Post -> String
longDate = formatTime defaultTimeLocale "%e %B, %Y" . date

-- Returns the post date, formatted like "2015-04-17".
shortDate :: Post -> String
shortDate = showGregorian . date

-- Returns the year in which the post was published.
year :: Post -> Integer
year post = y where (y, _m, _d) = toGregorian $ date post

-- Returns the canonical absolute url for a particular post.
url :: Post -> String
url post = "/" ++ datePath ++ "/" ++ (slug post)
  where datePath = formatTime defaultTimeLocale "%Y/%m/%d" $ date post

-- Returns whether post has code in it that requires a monospace font.
usesMonoFont :: Post -> Bool
usesMonoFont = not . null . Html.filterTags Html.isCode . Html.parseTags . body

-- Returns whether the post has <em> tags that require an italic font.
usesItalicFont :: Post -> Bool
usesItalicFont = not . null . Html.filterTags Html.isEm . Html.parseTags . body

-- Converts an integer to a Roman numeral (nothing fancy, works for 1-9).
toRoman :: Int -> String
toRoman i = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] !! (i - 1)

-- Maps a boolean to a field that can be used in a template expansion context.
booleanField :: Bool -> Maybe String
booleanField b = if b then Just "true" else Nothing

-- Given a string, and a substring consisting of two words, inserts a <br> tag
-- and a space between the two words in the original string.
addBreak :: String -> String -> String
addBreak between = Text.unpack . Text.replace textBetween broken . Text.pack
  where [brAfter, brBefore] = words between
        textBetween         = Text.pack between
        broken              = Text.pack $ brAfter ++ "<br> " ++ brBefore

-- Returns the template expansion context for the post.
context :: Post -> T.Context
context p = fmap T.StringValue ctx
  where ctx       = M.union fields (M.mapMaybe id optFields)
        fields    = M.fromList [ ("title", title p)
                               , ("header", header p)
                               , ("short-date", shortDate p)
                               , ("long-date", longDate p)
                               , ("url", url p)
                               , ("synopsis", synopsis p)
                               , ("content", body p) ]
        optFields = M.fromList [ ("subheader", subheader p)
                               , ("part", fmap toRoman $ part p)
                               , ("bold-font", boldFontField)
                               , ("italic-font", italicFontField)
                               , ("mono-font", monoFontField) ]
        boldFontField   = booleanField $ Type.usesBoldFont $ body p
        italicFontField = booleanField $ usesItalicFont p
        monoFontField   = booleanField $ usesMonoFont p

-- Given a slug and the contents of the post file (markdown with front matter),
-- renders the body to html and parses the metadata.
parse :: String -> String -> Post
parse postSlug contents = let
  (frontMatter, bodyContents) = extractFrontMatter contents
  postTitle     = frontMatter M.! "title"
  postHeading   = fromMaybe postTitle $ M.lookup "header" frontMatter
  breakAt       = M.lookup "break" frontMatter
  brokenHeading = foldr addBreak postHeading breakAt
  runIn         = M.lookup "run-in" frontMatter
  addRunIn html = foldl Html.makeRunIn html (fmap length runIn)
  refineType    = addRunIn . Type.expandPunctuation . Type.makeAbbrs
  parseDate     = parseTimeOrError True defaultTimeLocale "%F"
  in Post { title     = postTitle
          , header    = brokenHeading
          , subheader = M.lookup "subheader" frontMatter
          , part      = fmap read $ M.lookup "part" frontMatter
          , date      = parseDate $ frontMatter M.! "date"
          , slug      = postSlug
          , synopsis  = frontMatter M.! "synopsis"
          , body      = refineType $ renderMarkdown bodyContents }

-- Renders markdown to html using Pandoc with my settings.
renderMarkdown :: String -> String
renderMarkdown md = case fmap (writeHtmlString wopt) (readMarkdown ropt md) of
  Right result -> result
  Left  _      -> "Failed to parse markdown."
  -- Enable inline LaTeX between dollars, and enable backtick code blocks. Also
  -- accept tables. For output, enable syntax highlighting of code and write
  -- math as MathML.
  where ropt = def { readerExtensions     = S.insert Ext_backtick_code_blocks $
                                            S.insert Ext_simple_tables $
                                            S.insert Ext_tex_math_dollars
                                            def }
        wopt = def { writerHighlight      = True
                   , writerHTMLMathMethod = MathML Nothing }

-- Related content for a post, for the further reading section in the footer.
data RelatedContent = Further Post
                    | Series [Post]
                    deriving (Show) -- TODO: this is for debugging only, remove.

-- Returns the template expansion context for related content.
relatedContext :: RelatedContent -> T.Context
relatedContext related = case related of
  Further post -> T.nestContext "further" $ context post
  Series posts -> M.singleton "series" $ T.ListValue $ fmap context posts

-- Takes an (unordered) list of posts and produces a list of posts together with
-- related content for that post.
selectRelated :: [Post] -> [(Post, RelatedContent)]
selectRelated posts = fmap nextElsePrev prevPostNext
  where -- Create chronological triples of (previous post, post, next post).
        chronological = sortWith date posts
        prevPosts     = Nothing : (fmap Just chronological)
        nextPosts     = (drop 1 $ fmap Just chronological) ++ [Nothing]
        prevPostNext  = zip3 prevPosts chronological nextPosts

        -- Select the next post as "Further" content if there is one, otherwise
        -- take the previous post (which is assumed to exist in that case).
        nextElsePrev x = case x of
          (_, post, Just next) -> (post, Further next)
          (Just prev, post, _) -> (post, Further prev)
          _                    -> error "At least two posts are required."

-- Returns a context for a group of posts that share the same year.
archiveYearContext :: [Post] -> T.Context
archiveYearContext posts = M.fromList [yearField, postsField]
  where yearField     = ("year", T.StringValue $ show $ year $ head $ posts)
        chronological = sortWith date posts
        recentFirst   = reverse chronological
        postsField    = ("post", T.ListValue $ fmap context recentFirst)

-- Returns a contexts with a "archive-year" list where every year has a "posts"
-- lists.
archiveContext :: [Post] -> T.Context
archiveContext posts  = M.singleton "archive-year" (T.ListValue years)
  where yearGroups    = groupWith year posts
        chronological = sortWith (year . head) yearGroups
        recentFirst   = reverse chronological
        years         = fmap archiveYearContext recentFirst

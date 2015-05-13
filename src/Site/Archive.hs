module Site.Archive (archiveContext) where

import Control.Applicative (empty)
import Data.Function (on)
import Data.List (groupBy)
import Data.Monoid ((<>))
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Hakyll
import Site.Meta

-- The code below is inspired by code from Jorge Israel Peña’s site which is BSD-licensed.
-- https://github.com/blaenk/blaenk.github.io/blob/b22edf5c9ce4f9f1a5b429c3e565da8c13a12d2e/src/Site/Contexts.hs

groupedArchives :: Pattern -> Compiler [Item (Integer, [Item String])]
groupedArchives pat =
  mapM makeItem =<<
    map combineItems . groupBy ((==) `on` fst)
      <$> (mapM addYear =<< recentFirst =<< filterDraftItems =<< loadAll (pat .&&. hasNoVersion))
  where
    combineItems :: [(Integer, Item String)] -> (Integer, [Item String])
    combineItems = foldr (\(year, item) (_, items) -> (year, item : items)) (0, [])

    addYear :: Item String -> Compiler (Integer, Item String)
    addYear item = do
      year <- yearFromUTC <$> (getItemUTC defaultTimeLocale . itemIdentifier $ item)
      return (year, item)

    yearFromUTC :: UTCTime -> Integer
    yearFromUTC utcTime =
      let (year, _, _) = toGregorian $ utctDay utcTime
      in year

archiveContext :: Pattern -> Context a
archiveContext pattern =
  listField "years" (year <> posts) (groupedArchives pattern)
  where
    year  = field "year" (return . show . fst . itemBody)
    posts = Context $ \k _ i ->
              if k == "posts"
                then return $ ListField (postContext <> defaultContext) (snd . itemBody $ i)
                else empty

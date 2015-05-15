{-# LANGUAGE OverloadedStrings #-}

module Site.Index (postIndex) where

import Control.Monad (forM, forM_, liftM)
import Data.List (sortBy)
import Data.Maybe (catMaybes, isJust)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Time.Format (defaultTimeLocale)
import Hakyll
import Site.Meta

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)

postsField :: Compiler [Item String] -> Context a
postsField posts = listField "posts"
  (teaserField "teaser" "content" <> postContext <> defaultContext)
  posts

mathField :: Bool -> Context a
mathField True  = constField "math" "true"
mathField False = missingField

indexPageUrl :: Int -> FilePath
indexPageUrl 1 = "/index.html"
indexPageUrl i = "/page/" ++ show i ++ "/index.html"

-- Post index has 'older' and 'newer' fields.
postIndexContext :: Maybe FilePath -> Maybe FilePath -> Context String
postIndexContext older newer = 
  mconcat $ catMaybes [fmap (constField "older") older, fmap (constField "newer") newer]

paginatePosts :: Pattern -> Int -> (Maybe Int -> Int -> Maybe Int -> [Identifier] -> Rules ()) -> Rules ()
paginatePosts pattern n rules = do
  let sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
      sortByM f xs   = liftM (map fst . sortBy (comparing snd)) $
                       mapM (\x -> liftM (\y -> (x, y)) (f x)) xs
      chronological' = sortByM $ getItemUTC defaultTimeLocale
      recentFirst'   = liftM reverse . chronological'
  ids <- recentFirst' =<< filterDrafts =<< getMatches pattern
  let chunks     = chunksOf n ids
      indexPages = zip chunks [1..]
      lastIndex  = length indexPages
  forM_ indexPages $ \(ps, i) -> let newer = if i > 1         then Just (i - 1) else Nothing
                                     older = if i < lastIndex then Just (i + 1) else Nothing
                                 in  rules older i newer ps

postIndex :: Pattern -> Int -> Context String -> Rules ()
postIndex pattern nPages context =
  paginatePosts pattern nPages $ \older current newer ids -> do
      let olderUrl   = fmap indexPageUrl older
          newerUrl   = fmap indexPageUrl newer
          identifier = fromFilePath $ drop 1 $ indexPageUrl current
      create [identifier] $ do
        route idRoute
        compile $ do
          posts <- forM ids $ \postId -> loadSnapshot postId "content"
          maths <- forM ids $ \postId -> getMetadataField postId "math"
          let useMath  = any isJust maths
              indexCtx = postsField (return posts) <>
                postIndexContext olderUrl newerUrl <>
                mathField useMath                  <>
                context
        
          makeItem (indexPageUrl current)
            >>= loadAndApplyTemplate "templates/post-list.html" indexCtx
            >>= stripIndexSuffix

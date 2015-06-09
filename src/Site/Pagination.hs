module Site.Pagination (buildPaginate, paginatePostsContext) where

import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Traversable as T
import           Control.Monad (join, liftM)
import           Hakyll hiding (getMetadata)
import           Site.Meta

-- Taken from the source, because it is not exported.
paginateNumPages :: Paginate -> Int
paginateNumPages = M.size . paginateMap

-- Taken from the source, because it is not exported.
paginatePage :: Paginate -> PageNumber -> Maybe Identifier
paginatePage pag pageNumber
 | pageNumber < 1 = Nothing
 | pageNumber > (paginateNumPages pag) = Nothing
 | otherwise = Just $ paginateMakeId pag pageNumber

getMetadata :: MonadMetadata m
            => Paginate -> PageNumber -> String -> m (Maybe String)
getMetadata pag i key = liftM join $ T.sequence $ do
  ident <- paginatePage pag i
  return $ getMetadataField ident key

-- Context that has title fields extracted from metadata.
paginateMetadataContext :: Paginate -> PageNumber -> Context a
paginateMetadataContext pag i = mconcat
  [ field "nextTitle"     $ \_ -> fmap (maybe "" id) $ getMetadata pag (i + 1) "title"
  , field "previousTitle" $ \_ -> fmap (maybe "" id) $ getMetadata pag (i - 1) "title"
  ]

paginatePostsContext :: Paginate -> PageNumber -> Context String
paginatePostsContext pages i = postContext <>
                               paginateContext pages i <>
                               paginateMetadataContext pages i

-- Pagination that takes drafts into account, adapted from the original source.
buildPaginate :: MonadMetadata m => Pattern -> m Paginate
buildPaginate pattern = do
  ids      <- filterDrafts =<< getMatches pattern
  idGroups <- return $ fmap (\x -> [x]) ids -- Just one page per post.
  let idsSet = S.fromList ids
  return Paginate
    { paginateMap        = M.fromList (zip [1 ..] idGroups)
    , paginateMakeId     = \i -> ids !! (i-1) -- Page url is just the post url.
    , paginateDependency = PatternDependency pattern idsSet
    }

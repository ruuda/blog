module Site.Pagination (buildPaginate, paginatePostsContext) where

import qualified Data.Map as M
import           Data.Monoid ((<>), mconcat)
import qualified Data.Set as S
import           Hakyll hiding (buildPaginate)
import           Site.Meta
import           Text.Printf (printf)

-- Re-define relpage, because it is not exported.
type RelPage = PageNumber -> PageNumber -> PageNumber -> Maybe PageNumber

-- Custom pagination with metadata extraction.
paginateMetadataField :: Paginate -> String -> RelPage -> String -> Context a
paginateMetadataField pag fieldName relPage key = field fieldName $ \item ->
  let identifier = itemIdentifier item
  in case M.lookup identifier (paginatePlaces pag) of
     Nothing  -> fail $ printf
       "Hakyll.Web.Paginate: there is no page %s in paginator map."
       (show identifier)
     Just pos -> case relPage 1 pos nPages of
       Nothing   -> fail "Hakyll.Web.Paginate: No page here."
       Just pos' -> do
         let nextId = paginateMakeId pag pos'
         getMetadataField' nextId key
  where
    nPages = M.size (paginatePages pag)

-- Context that has title fields extracted from metadata.
paginateMetadataContext :: Paginate -> Context a
paginateMetadataContext pag = mconcat
  [ paginateMetadataField pag "nextTitle"
      (\_ c l -> if c >= l then Nothing else Just (c + 1)) "title"
  , paginateMetadataField pag "previousTitle"
      (\f c _ -> if c <= f then Nothing else Just (c - 1)) "title"
  ]

paginatePostsContext :: Paginate -> Context String
paginatePostsContext pages = postContext <>
                             paginateContext pages <>
                             paginateMetadataContext pages

-- Pagination that takes drafts into account, adapted from the original source.
buildPaginate :: MonadMetadata m => Pattern -> m Paginate
buildPaginate pattern = do
    idents <- filterDrafts =<< getMatches pattern
    let pagPages  = M.fromList $ zip [1 ..] (map return idents)
        pagPlaces = M.fromList $ zip idents [1 ..]
        makeId pn = case M.lookup pn pagPages of
            Just [id'] -> id'
            _          -> error $
                "Hakyll.Web.Paginate.buildPaginate': " ++
                "invalid page number: " ++ show pn

    return $ Paginate pagPages pagPlaces makeId
        (PatternDependency pattern (S.fromList idents))
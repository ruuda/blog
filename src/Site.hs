{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import GHC.IO.Encoding
import Hakyll hiding (buildPaginate, pandocCompiler)
import Site.Archive
import Site.Configuration
import Site.Index
import Site.Meta
import Site.Pagination
import Site.Pandoc

staticFile :: Pattern -> Rules ()
staticFile pattern = do
  match pattern $ do
    route idRoute
    compile copyFileCompiler

main :: IO ()
main = do
  -- Fix encoding on Windows
  setLocaleEncoding utf8

  -- Retrieve current year to put in the footer
  yearContext <- getYearContext
  let siteContext = yearContext <> defaultContext
  let fullContext = constField "title" "Ruud van Asseldonk" <> siteContext

  -- Run Hakyll
  hakyllWith config $ do

    match "style/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    sequence_ $ fmap staticFile
      [ "style/*.woff", "style/*.png", "images/*", "favicon.png", ".htaccess" ]

    match "templates/*" $ compile templateCompiler

    match "posts/*.md" $ do
      route dateRoute
      compile $ do
        pages <- buildPaginate "posts/*.md"
        let ctx = paginatePostsContext pages <> siteContext
        pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= stripIndexSuffix
    
    postIndex "posts/*.md" 5 fullContext

    create ["archive/index.html"] $ do
      route idRoute
      compile $ do
        let ctx = archiveContext "posts/*" <> fullContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" ctx
          >>= stripIndexSuffix

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        let feedContext = bodyField "description" <> defaultContext
        loadAllSnapshots "posts/*" "content"
          >>= filterDraftItems
          >>= fmap (take 10) . recentFirst
          >>= renderAtom feedConfig feedContext

-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import Control.Monad (filterM, void)
import Data.Monoid ((<>))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Directory (doesFileExist, copyFile, createDirectoryIfMissing, getDirectoryContents)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension, takeFileName)
import System.Process

import qualified Data.Map as M
import qualified Control.Concurrent.Async as Async

import Type (SubsetCommand, subsetArtifact, subsetFonts)
import Minification (minifyHtml)

import qualified Image
import qualified Post as P
import qualified Template

-- Applies the IO-performing function f to every file in a given directory if
-- the filename satisfies the predicate p.
mapFilesIf :: (FilePath -> Bool) -> (FilePath -> IO a) -> FilePath -> IO [a]
mapFilesIf p f dir = enumerateFiles >>= filterM doesFileExist >>= mapM f
  -- Prepend the directory names to the names returned by getDirectoryContents.
  where enumerateFiles = fmap (filter p . fmap (dir </>)) $ getDirectoryContents dir

-- Applies the IO-performing function f to every file in a given directory.
mapFiles :: (FilePath -> IO a) -> FilePath -> IO [a]
mapFiles = mapFilesIf $ \_ -> True

-- Copies all files in the source directory to the destination directory.
copyFiles :: FilePath -> FilePath -> IO ()
copyFiles srcDir dstDir = void $ mapFiles copy srcDir
  where copy fname = copyFile fname $ dstDir </> (takeFileName fname)

-- Applies the IO-performing function f to every file in a given directory, and
-- returns a map from the file name to the result.
mapFilesFileName :: (FilePath -> IO a) -> FilePath -> IO (M.Map FilePath a)
mapFilesFileName f = (fmap M.fromList) . (mapFiles makePair)
  where makePair fname = fmap (\x -> (takeFileName fname, x)) (f fname)

-- Reads and parses all templates in the given directory.
readTemplates :: FilePath -> IO (M.Map FilePath Template.Template)
readTemplates = mapFilesFileName $ (fmap Template.parse) . readFile

-- Reads a post from a file.
readPost :: FilePath -> IO P.Post
readPost fname = fmap makePost $ readFile fname
  where makePost body = P.parse (takeBaseName fname) body

-- Reads and renders all posts in the given directory.
readPosts :: FilePath -> IO [P.Post]
readPosts = mapFilesIf ((== ".md") . takeExtension) readPost

-- Holds the output directory and input image directory.
data Config = Config { outDir   :: FilePath
                     , imageDir :: FilePath }

-- Compresses the given file to a new file with .gz/br appended to the filename.
compressFile :: FilePath -> IO ()
compressFile fname = do
  System.Process.callProcess "zopfli" [fname]
  System.Process.callProcess "brotli" ["--force", "--output=" ++ fname ++ ".br", fname]

-- Given the post template and the global context, expands the template for all
-- of the posts and writes them to the output directory. This also prints a list
-- of processed posts to the standard output. Start numbering post artifacts at
-- 53, lower indices are reserved for other pages.
writePosts :: Template.Template -> Template.Context -> [P.Post] -> Config -> IO [SubsetCommand]
writePosts tmpl ctx posts config =
  let
    total = length posts
    -- Reverse the list of posts, so the most recent one is rendered first.
    -- This makes the preview workflow faster, because the most recent post
    -- in the list is likely the one that I want to view.
    withRelated = zip [1 :: Int ..] $ reverse $ P.selectRelated posts
    writePostAsync (i, (post, related)) = do
      putStrLn $ "[" ++ (show i) ++ " of " ++ (show total) ++ "] " ++ (P.slug post)
      Async.async $ writePost post related
    writePost post related = do
      let destFile = (outDir config) </> (drop 1 $ P.url post) </> "index.html"
          context  = M.unions [ P.context post
                              , P.relatedContext related
                              , ctx]
          -- Generating the html is a two-stage process: first we render the
          -- template without knowing the font filenames, as those are based
          -- on the hash of the glyphs. Then we scan which glyphs occur in
          -- there to determine the hash, and then we can render again.
          baseHtml = Template.apply tmpl context
          (fontCtx, subsetCmds) = Type.subsetArtifact "out/fonts/" baseHtml
          html = Template.apply tmpl (context <> fontCtx)
      withImages  <- Image.processImages (imageDir config) html
      let minified = minifyHtml withImages
      createDirectoryIfMissing True $ takeDirectory destFile
      writeFile destFile minified
      compressFile destFile
      pure subsetCmds
  in do
    subsetCmdsAsync <- mapM writePostAsync withRelated
    subsetCmds <- mapM Async.wait subsetCmdsAsync
    pure (concat subsetCmds)

-- Writes a general (non-post) page given a template and expansion context.
-- Returns the subset commands that need to be executed for that page.
writePage :: String -> Template.Context -> Template.Template -> Config -> IO [SubsetCommand]
writePage url pageContext template config = do
  let context  = Template.stringField "url" url <> pageContext
      baseHtml = Template.apply template context
      (fontCtx, subsetCmds) = Type.subsetArtifact "out/fonts/" baseHtml
      html     = minifyHtml $ Template.apply template (context <> fontCtx)
      destDir  = (outDir config) </> (tail url)
      destFile = destDir </> "index.html"
  createDirectoryIfMissing True destDir
  writeFile destFile html
  compressFile destFile
  pure subsetCmds

writeIndex :: Template.Context -> Template.Template -> Config -> IO [SubsetCommand]
writeIndex globalContext = writePage "/" context
  where context = M.unions [ Template.stringField "title"     "Ruud van Asseldonk"
                           , Template.stringField "bold-font" "true"
                           , Template.stringField "light"     "true"
                           , globalContext ]

-- Given the archive template and the global context, writes the archive page
-- to the destination directory.
writeArchive :: Template.Context -> Template.Template -> [P.Post] -> Config -> IO [SubsetCommand]
writeArchive globalContext template posts = writePage "/writing" context template
  where context = M.unions [ P.archiveContext posts
                           , Template.stringField "title"     "Writing by Ruud van Asseldonk"
                           , Template.stringField "bold-font" "true"
                           , Template.stringField "archive"   "true"
                           , globalContext ]

-- Given the contact template and the global context, writes the contact page
-- to the destination directory.
writeContact :: Template.Context -> Template.Template -> Config -> IO [SubsetCommand]
writeContact globalContext = writePage "/contact" context
  where context = M.unions [ Template.stringField "title" "Contact Ruud van Asseldonk"
                           , Template.stringField "light" "true"
                           , globalContext ]

-- Given the feed template and list of posts, writes an atom feed.
writeFeed :: Template.Template -> [P.Post] -> Config -> IO ()
writeFeed template posts config = do
  let url      = "/feed.xml"
      context  = P.feedContext posts
      atom     = Template.apply template context
      destFile = (outDir config) </> (tail url)
  createDirectoryIfMissing True (outDir config)
  writeFile destFile atom
  compressFile destFile

main :: IO ()
main = do
  templates <- readTemplates "templates/"
  posts     <- readPosts     "posts/"

  -- Create a context with the field "year" set to the current year, and create
  -- a context that contains all of the templates, to handle includes.
  (year, _month, _day) <- fmap (toGregorian . utctDay) getCurrentTime
  let yctx          = Template.stringField "year" $ show year
      tctx          = fmap Template.TemplateValue templates
      globalContext = M.union tctx yctx
      config        = Config { outDir   = "out/"
                             , imageDir = "images/compressed/" }

  createDirectoryIfMissing True  "out/images/"
  copyFiles "images/compressed/" "out/images/"

  putStrLn "Writing posts..."
  postCmds <- writePosts (templates M.! "post.html") globalContext posts config

  putStrLn "Writing other pages..."
  indexCmd   <- writeIndex   globalContext (templates M.! "index.html")   config
  contactCmd <- writeContact globalContext (templates M.! "contact.html") config
  archiveCmd <- writeArchive globalContext (templates M.! "archive.html") posts config

  copyFile "assets/favicon.png"          "out/favicon.png"
  copyFile "assets/ruudvanasseldonk.asc" "out/contact/ruudvanasseldonk.asc"

  putStrLn "Writing atom feed..."
  writeFeed (templates M.! "feed.xml") posts config

  putStrLn "Subsetting fonts..."
  createDirectoryIfMissing True "out/fonts"
  nSubsetted <- subsetFonts $ concat [indexCmd, contactCmd, archiveCmd, postCmds]
  putStrLn $ "Subsetted " ++ show nSubsetted ++ " new fonts."

-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import Control.Monad (filterM)
import Data.List (isSuffixOf, sortOn)
import Data.Monoid ((<>))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Directory (doesFileExist, copyFile, createDirectoryIfMissing, getDirectoryContents)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension, takeFileName)
import System.Process
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetContents, hPutStr, hSetEncoding, openFile, utf8)

import qualified Data.Map as M
import qualified Control.Concurrent.Async as Async
import qualified System.Process as Process

import Type (SubsetCommand, subsetArtifact, subsetFonts)
import Minification (minifyHtml)

import qualified Image
import qualified Post as P
import qualified Template

-- Read a file in UTF-8 encoding.
readUtf8 :: FilePath -> IO String
readUtf8 path = do
  handle <- openFile path ReadMode
  hSetEncoding handle utf8
  hGetContents handle

-- Write a file in UTF-8 encoding.
writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path contents = do
  handle <- openFile path WriteMode
  hSetEncoding handle utf8
  hPutStr handle contents
  hClose handle

-- Applies the IO-performing function f to every file in a given directory if
-- the filename satisfies the predicate p.
mapFilesIf :: (FilePath -> Bool) -> (FilePath -> IO a) -> FilePath -> IO [a]
mapFilesIf p f dir = enumerateFiles >>= filterM doesFileExist >>= mapM f
  -- Prepend the directory names to the names returned by getDirectoryContents.
  where enumerateFiles = fmap (filter p . fmap (dir </>)) $ getDirectoryContents dir

-- Applies the IO-performing function f to every file in a given directory.
mapFiles :: (FilePath -> IO a) -> FilePath -> IO [a]
mapFiles = mapFilesIf $ \_ -> True

-- Applies the IO-performing function f to every file in a given directory, and
-- returns a map from the file name to the result.
mapFilesFileName :: (FilePath -> IO a) -> FilePath -> IO (M.Map FilePath a)
mapFilesFileName f = (fmap M.fromList) . (mapFiles makePair)
  where makePair fname = fmap (\x -> (takeFileName fname, x)) (f fname)

-- Reads and parses all templates in the given directory.
readTemplates :: FilePath -> IO (M.Map FilePath Template.Template)
readTemplates = mapFilesFileName $ (fmap Template.parse) . readUtf8

-- Reads a post from a file.
readPost :: FilePath -> IO P.Post
readPost fname = fmap makePost $ readUtf8 fname
  where makePost body = P.parse (takeBaseName fname) body

-- Read a .md.m4 document, apply m4, then read it as a normal post.
readPostM4 :: FilePath -> IO P.Post
readPostM4 fname =
  let
    m4 = (Process.proc "m4" [fname]) { Process.std_out = Process.CreatePipe }
  in do
    (_stdin, Just hstdout, _stderr, handle) <- Process.createProcess m4
    hSetEncoding hstdout utf8
    body <- hGetContents hstdout
    -- Note the double takeBaseName, one to strip .m4, one to strip .md.
    pure $ P.parse (takeBaseName $ takeBaseName fname) body

-- Reads and renders all posts in the given directory.
readPosts :: FilePath -> IO [P.Post]
readPosts = do
  rawMd <- mapFilesIf ((== ".md") . takeExtension) readPost
  m4Md  <- mapFilesIf ((== ".m4") . takeExtension) readPostM4
  pure $ rawMd <> m4Md

-- Copy over bitmap images, render svg images, so they can reference
-- subsetted fonts. Svgs should be rendered with the font context of their post.
writeImage :: Template.Context -> FilePath -> IO ()
writeImage ctx fname =
  let
    inFile = "images/compressed" </> fname
    outFile = "out/images" </> fname
    copyBitmap = copyFile inFile outFile
    renderSvg = do
      template <- Template.parse <$> readUtf8 inFile
      writeUtf8 outFile (Template.apply template ctx)
      compressFile outFile
  in do
    if ".svg" `isSuffixOf` fname then renderSvg else copyBitmap

-- Compresses the given file to a new file with .gz/br appended to the filename.
compressFile :: FilePath -> IO ()
compressFile fname = do
  -- NOTE: I could squeeze out a few more bytes here with "zopfli --i1000"
  -- rather than the default number of iterations, but it's really no more
  -- than ten bytes per page, and it does increase the build time by 20 seconds
  -- or so, so I don't think that is really worth it.
  System.Process.callProcess "zopfli" [fname]
  System.Process.callProcess "brotli" ["--force", "--output=" ++ fname ++ ".br", fname]

-- Given the post template and the global context, expands the template for all
-- of the posts and writes them to the output directory. This also prints a list
-- of processed posts to the standard output. Start numbering post artifacts at
-- 53, lower indices are reserved for other pages.
writePosts :: Template.Template -> Template.Context -> [P.Post] -> IO [SubsetCommand]
writePosts tmpl ctx posts =
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
      let
        destFile = "out" </> (drop 1 $ P.url post) </> "index.html"
        context  = M.unions [ P.context post
                            , P.relatedContext related
                            , ctx]
        -- Generating the html is a two-stage process: first we render the
        -- template without knowing the font filenames, as those are based
        -- on the hash of the glyphs. Then we scan which glyphs occur in
        -- there to determine the hash, and then we can render again.
        baseHtml = (Template.apply tmpl context) ++ (P.extraGlyphs post)
        (fontCtx, subsetCmds) = Type.subsetArtifact "out/fonts/" baseHtml
        html = Template.apply tmpl (context <> fontCtx)
      (imgPaths, withImages) <- Image.processImages "images/compressed" html
      -- Copy referenced bitmap images, render svg images as templates.
      mapM_ (writeImage fontCtx) imgPaths
      let minified = minifyHtml withImages
      createDirectoryIfMissing True $ takeDirectory destFile
      writeUtf8 destFile minified
      compressFile destFile
      pure subsetCmds
  in do
    subsetCmdsAsync <- mapM writePostAsync withRelated
    subsetCmds <- mapM Async.wait subsetCmdsAsync
    pure (concat subsetCmds)

-- Writes a general (non-post) page given a template and expansion context.
-- Returns the subset commands that need to be executed for that page.
writePage :: String -> Template.Context -> Template.Template -> IO [SubsetCommand]
writePage url pageContext template = do
  let context  = Template.stringField "url" url <> pageContext
      baseHtml = Template.apply template context
      (fontCtx, subsetCmds) = Type.subsetArtifact "out/fonts/" baseHtml
      html     = minifyHtml $ Template.apply template (context <> fontCtx)
      destDir  = "out" </> (tail url)
      destFile = destDir </> "index.html"
  createDirectoryIfMissing True destDir
  writeUtf8 destFile html
  compressFile destFile
  pure subsetCmds

writeIndex :: Template.Context -> Template.Template -> IO [SubsetCommand]
writeIndex globalContext = writePage "/" context
  where
    context = M.unions
      [ Template.stringField "title"     "Ruud van Asseldonk"
      , Template.stringField "bold-font" "true"
      , Template.stringField "light"     "true"
        -- My intro is in British English
      , Template.stringField "lang"      "en-GB"
      , globalContext
      ]

-- Given the archive template and the global context, writes the archive page
-- to the destination directory.
writeArchive :: Template.Context -> Template.Template -> [P.Post] -> IO [SubsetCommand]
writeArchive globalContext template posts = writePage "/writing" context template
  where
    context = M.unions
      [ P.archiveContext posts
      , Template.stringField "title"     "Writing by Ruud van Asseldonk"
      , Template.stringField "bold-font" "true"
      , Template.stringField "archive"   "true"
        -- The summaries mix British and US English, so don't be specific.
      , Template.stringField "lang"      "en"
      , globalContext
      ]

-- Given the contact template and the global context, writes the contact page
-- to the destination directory.
writeContact :: Template.Context -> Template.Template -> IO [SubsetCommand]
writeContact globalContext = writePage "/contact" context
  where
    context = M.unions
      [ Template.stringField "title" "Contact Ruud van Asseldonk"
      , Template.stringField "light" "true"
      , Template.stringField "lang"  "en-GB"
      , globalContext
      ]

-- Given the feed template and list of posts, writes an atom feed.
writeFeed :: Template.Template -> [P.Post] -> IO ()
writeFeed template posts = do
  let url      = "/feed.xml"
      context  = P.feedContext posts
      atom     = Template.apply template context
      destFile = "out" </> (tail url)
  createDirectoryIfMissing True "out"
  writeUtf8 destFile atom
  compressFile destFile

sortPosts :: [P.Post] -> [P.Post]
sortPosts = sortOn $ \p -> (P.date p, P.part p)

main :: IO ()
main = do
  templates <- readTemplates "templates/"
  posts <- sortPosts <$> readPosts "posts/"

  -- Create a context with the field "year" set to the current year, and create
  -- a context that contains all of the templates, to handle includes.
  (year, _month, _day) <- fmap (toGregorian . utctDay) getCurrentTime
  let yctx          = Template.stringField "year" $ show year
      tctx          = fmap Template.TemplateValue templates
      globalContext = M.union tctx yctx

  createDirectoryIfMissing True  "out/images/"

  putStrLn "Writing posts..."
  postCmds <- writePosts (templates M.! "post.html") globalContext posts

  putStrLn "Writing other pages..."
  indexCmd   <- writeIndex   globalContext (templates M.! "index.html")
  contactCmd <- writeContact globalContext (templates M.! "contact.html")
  archiveCmd <- writeArchive globalContext (templates M.! "archive.html") posts

  copyFile "assets/favicon.png"          "out/favicon.png"
  copyFile "assets/ruudvanasseldonk.asc" "out/contact/ruudvanasseldonk.asc"

  putStrLn "Writing atom feed..."
  writeFeed (templates M.! "feed.xml") posts

  putStrLn "Subsetting fonts..."
  createDirectoryIfMissing True "out/fonts"
  nSubsetted <- subsetFonts $ concat [indexCmd, contactCmd, archiveCmd, postCmds]
  putStrLn $ "Subsetted " ++ show nSubsetted ++ " new fonts."

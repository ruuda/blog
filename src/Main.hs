-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import           Control.Monad (filterM, foldM, void)
import qualified Data.Map as M
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           System.Directory (doesFileExist, copyFile, createDirectoryIfMissing, getDirectoryContents)
import           System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension, takeFileName)

import qualified Image
import           Minification (minifyHtml)
import qualified Post as P
import qualified Template
import           Type (subsetArtifact, subsetFonts)

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

-- An artifact is a numbered page plus its html contents.
type Artifact = (Int, String)

pageIdContext :: Int -> Template.Context
pageIdContext i = Template.stringField "page-id" $ show i

-- Holds the output directory and input image directory.
data Config = Config { outDir   :: FilePath
                     , imageDir :: FilePath }

-- Given the post template and the global context, expands the template for all
-- of the posts and writes them to the output directory. This also prints a list
-- of processed posts to the standard output. Start numbering post artifacts at
-- 53, lower indices are reserved for other pages.
writePosts :: Template.Template -> Template.Context -> [P.Post] -> Config -> IO [Artifact]
writePosts tmpl ctx posts config = fmap snd $ foldM writePost (1, []) withRelated
  where total       = length posts
        pageIdSeed  = 52
        withRelated = P.selectRelated posts
        writePost (i, artifacts) (post, related) = do
          let destFile = (outDir config) </> (drop 1 $ P.url post) </> "index.html"
              pageId   = pageIdSeed + i
              context  = M.unions [ P.context post
                                  , P.relatedContext related
                                  , pageIdContext pageId
                                  , ctx]
              html     = Template.apply tmpl context
          withImages  <- Image.processImages (imageDir config) html
          let minified = minifyHtml withImages
              artifact = (pageId, minified)
          putStrLn $ "[" ++ (show i) ++ " of " ++ (show total) ++ "] " ++ (P.slug post)
          createDirectoryIfMissing True $ takeDirectory destFile
          writeFile destFile minified
          return $ (i + 1, artifact:artifacts)

-- Writes a general (non-post) page given a template and expansion context.
writePage :: Int -> String -> Template.Context -> Template.Template -> Config -> IO Artifact
writePage pageId url pageContext template config = do
  let context  = M.unions [ Template.stringField "url" url
                          , pageIdContext pageId
                          , pageContext ]
      html     = minifyHtml $ Template.apply template context
      artifact = (pageId, html)
      destDir  = (outDir config) </> (tail url)
  createDirectoryIfMissing True destDir
  writeFile (destDir </> "index.html") html
  return artifact

writeIndex :: Template.Context -> Template.Template -> Config -> IO Artifact
writeIndex globalContext = writePage 0 "/" context
  where context = M.unions [ Template.stringField "title"     "Ruud van Asseldonk"
                           , Template.stringField "bold-font" "true"
                           , Template.stringField "light"     "true"
                           , globalContext ]

-- Given the archive template and the global context, writes the archive page
-- to the destination directory.
writeArchive :: Template.Context -> Template.Template -> [P.Post] -> Config -> IO Artifact
writeArchive globalContext template posts = writePage 1 "/writing" context template
  where context = M.unions [ P.archiveContext posts
                           , Template.stringField "title"     "Writing by Ruud van Asseldonk"
                           , Template.stringField "bold-font" "true"
                           , globalContext ]

-- Given the contact template and the global context, writes the contact page
-- to the destination directory.
writeContact :: Template.Context -> Template.Template -> Config -> IO Artifact
writeContact globalContext = writePage 2 "/contact" context
  where context = M.unions [ Template.stringField "title"     "Contact Ruud van Asseldonk"
                           , Template.stringField "mono-font" "true"
                           , Template.stringField "light"     "true"
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

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

-- Subsets fonts for every page, putting subsetted fonts in the specified
-- directory with a name based on the post number and a font-specific suffix.
subsetFontsForArtifacts :: [Artifact] -> FilePath -> IO ()
subsetFontsForArtifacts artifacts fontDir = do
  createDirectoryIfMissing True fontDir
  subsetFonts $ concatMap (uncurry subsetArtifact) namedArtifacts
  where namedArtifacts = fmap (mapFst $ \i -> fontDir ++ (show i)) artifacts

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
  postArtifacts <- writePosts (templates M.! "post.html") globalContext posts config

  putStrLn "Writing other pages..."
  indexArtifact   <- writeIndex   globalContext (templates M.! "index.html")   config
  contactArtifact <- writeContact globalContext (templates M.! "contact.html") config
  archiveArtifact <- writeArchive globalContext (templates M.! "archive.html") posts config

  copyFile "assets/.htaccess"            "out/.htaccess"
  copyFile "assets/favicon.png"          "out/favicon.png"
  copyFile "assets/ruudvanasseldonk.asc" "out/contact/ruudvanasseldonk.asc"

  putStrLn "Writing atom feed..."
  writeFeed (templates M.! "feed.xml") posts config

  putStrLn "Subsetting fonts..."
  let artifacts = indexArtifact : contactArtifact : archiveArtifact : postArtifacts
  subsetFontsForArtifacts artifacts "out/fonts/"

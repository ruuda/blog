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
import qualified Template as T
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
readTemplates :: FilePath -> IO (M.Map FilePath T.Template)
readTemplates = mapFilesFileName $ (fmap T.parse) . readFile

-- Reads a post from a file.
readPost :: FilePath -> IO P.Post
readPost fname = fmap makePost $ readFile fname
  where makePost body = P.parse (takeBaseName fname) body

-- Reads and renders all posts in the given directory.
readPosts :: FilePath -> IO [P.Post]
readPosts = mapFilesIf ((== ".md") . takeExtension) readPost

-- An artifact is a numbered page plus its html contents.
type Artifact = (Int, String)

pageIdContext :: Int -> T.Context
pageIdContext i = M.singleton "page-id" (T.StringValue $ show i)

-- Holds the output directory and input image directory.
data Config = Config { outDir   :: FilePath
                     , imageDir :: FilePath }

-- Given the post template and the global context, expands the template for all
-- of the posts and writes them to the output directory. This also prints a list
-- of processed posts to the standard output. Start numbering post artifacts at
-- 53, lower indices are reserved for other pages.
writePosts :: T.Template -> T.Context -> [P.Post] -> Config -> IO [Artifact]
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
              html     = T.apply tmpl context
          withImages  <- Image.processImages (imageDir config) html
          let minified = minifyHtml withImages
              artifact = (pageId, minified)
          putStrLn $ "[" ++ (show i) ++ " of " ++ (show total) ++ "] " ++ (P.slug post)
          createDirectoryIfMissing True $ takeDirectory destFile
          writeFile destFile minified
          return $ (i + 1, artifact:artifacts)

-- Writes a general (non-post) page given a template and expansion context.
writePage :: Int -> String -> T.Context -> T.Template -> Config -> IO Artifact
writePage pageId url pageContext template config = do
  let context  = M.unions [ M.singleton "url" (T.StringValue url)
                          , pageIdContext pageId
                          , pageContext ]
      html     = minifyHtml $ T.apply template context
      artifact = (pageId, html)
      destDir  = (outDir config) </> (tail url)
  createDirectoryIfMissing True destDir
  writeFile (destDir </> "index.html") html
  return artifact

-- Given the archive template and the global context, writes the archive page
-- to the destination directory.
writeArchive :: T.Context -> T.Template -> [P.Post] -> Config -> IO Artifact
writeArchive globalContext template posts = writePage 0 "/writing" context template
  where context = M.unions [ P.archiveContext posts
                           , M.singleton "title"     (T.StringValue "Writing by Ruud van Asseldonk")
                           , M.singleton "bold-font" (T.StringValue "true")
                           , globalContext ]

-- Given the contact template and the global context, writes the contact page
-- to the destination directory.
writeContact :: T.Context -> T.Template -> Config -> IO Artifact
writeContact globalContext = writePage 1 "/contact" context
  where context = M.unions [ M.singleton "title"     (T.StringValue "Contact Ruud van Asseldonk")
                           , M.singleton "mono-font" (T.StringValue "true")
                           , globalContext ]

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
  let yctx          = M.singleton "year" (T.StringValue $ show year)
      tctx          = fmap T.TemplateValue templates
      globalContext = M.union tctx yctx
      config        = Config { outDir   = "out/"
                             , imageDir = "images/compressed/" }

  createDirectoryIfMissing True  "out/images/"
  copyFiles "images/compressed/" "out/images/"

  putStrLn "Writing posts..."
  postArtifacts <- writePosts (templates M.! "post.html") globalContext posts config

  putStrLn "Writing archive..."
  archiveArtifact <- writeArchive globalContext (templates M.! "archive.html") posts config
  contactArtifact <- writeContact globalContext (templates M.! "contact.html") config

  putStrLn "Subsetting fonts..."
  let artifacts = archiveArtifact : contactArtifact : postArtifacts
  subsetFontsForArtifacts artifacts "out/fonts/"

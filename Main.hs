-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import           Control.Monad (filterM, mapM, foldM)
import qualified Data.Map as M
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           System.Directory (doesFileExist, createDirectoryIfMissing, getDirectoryContents)
import           System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension, takeFileName)

import           Font (subsetArtifact, subsetFonts)
import           Minification (minifyHtml)
import qualified Post as P
import qualified Template as T

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
readTemplates :: FilePath -> IO (M.Map FilePath T.Template)
readTemplates = mapFilesFileName $ (fmap T.parse) . readFile

-- Reads a post from a file.
readPost :: FilePath -> IO P.Post
readPost fname = fmap makePost $ readFile fname
  where makePost body = P.parse (takeBaseName fname) body

-- Reads and renders all posts in the given directory.
readPosts :: FilePath -> IO [P.Post]
readPosts = mapFilesIf ((== ".md") . takeExtension) readPost

-- An artifact is a numbered post plus its html contents.
type Artifact = (Int, String)

pageIdContext :: Int -> T.Context
pageIdContext i = M.singleton "page-id" (T.StringValue $ show i)

-- Given the post template and the global context, expands the template for all
-- of the posts and writes them to the output directory. This also prints a list
-- of processed posts to the standard output.
writePosts :: T.Template -> T.Context -> [P.Post] -> FilePath -> IO [Artifact]
writePosts tmpl ctx posts outDir = fmap snd $ foldM writePost (1, []) withRelated
  where total       = length posts
        withRelated = P.selectRelated posts
        writePost (i, artifacts) (post, related) = do
          let destFile = outDir </> (drop 1 $ P.url post) </> "index.html"
              context  = M.unions [ P.context post
                                  , P.relatedContext related
                                  , pageIdContext i
                                  , ctx]
              rendered = minifyHtml $ T.apply tmpl context
              artifact = (i, rendered)
          putStrLn $ "[" ++ (show i) ++ " of " ++ (show total) ++ "] " ++ (P.slug post)
          createDirectoryIfMissing True $ takeDirectory destFile
          writeFile destFile rendered
          return $ (i + 1, artifact:artifacts)

main :: IO ()
main = do
  templates <- readTemplates "templates/"
  posts     <- readPosts     "posts/"

  -- Create a context with the field "year" set to the current year, and create
  -- a context that contains all of the templates, to handle includes.
  (year, month, day) <- fmap (toGregorian . utctDay) getCurrentTime
  let yctx = M.singleton "year" (T.StringValue $ show year)
      tctx = fmap T.TemplateValue templates
      globalContext = M.union tctx yctx

  putStrLn "Writing posts..."
  artifacts <- writePosts (templates M.! "post.html") globalContext posts "out/"

-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import           Control.Monad (filterM, mapM)
import qualified Data.Map as M
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.FilePath ((</>), takeBaseName)

import qualified Post as P
import qualified Template as T

-- Applies the IO-performing function f to every file in a given directory.
mapFiles :: (FilePath -> IO a) -> FilePath -> IO [a]
mapFiles f dir = enumerateFiles >>= filterM doesFileExist >>= mapM f
  -- Prepend the directory names to the names returned by getDirectoryContents.
  where enumerateFiles = fmap (fmap (dir </>)) (getDirectoryContents dir)

-- Applies the IO-performing function f to every file in a given directory, and
-- returns a map from the file basename to the result.
mapFilesBaseName :: (FilePath -> IO a) -> FilePath -> IO (M.Map String a)
mapFilesBaseName f = (fmap M.fromList) . (mapFiles makePair)
  where makePair fname = fmap (\x -> (takeBaseName fname, x)) (f fname)

-- Reads and parses all templates in the given directory.
readTemplate :: FilePath -> IO T.Template
readTemplate = (fmap T.parse) . readFile

-- Reads a post from a file.
readPost :: FilePath -> IO P.Post
readPost fname = fmap makePost $ readFile fname
  where makePost body = P.parse (takeBaseName fname) body

-- Reads and renders all posts in the given directory.
readPosts :: FilePath -> IO [P.Post]
readPosts = mapFiles readPost

-- Applies the inner template, sets that as body for the outer template,
-- and applies the outer template.
applyTemplates :: T.Template -> T.Template -> T.Context -> String
applyTemplates outer inner context = T.apply outer octx
  where octx = M.insert "body" (T.StringValue $ T.apply inner context) context

main :: IO ()
main = do
  baseTmpl <- readTemplate "templates/base.html"
  postTmpl <- readTemplate "templates/post.html"
  posts    <- readPosts    "posts/"

  let expandPost = applyTemplates baseTmpl postTmpl

  putStrLn "TODO: actually generate something"

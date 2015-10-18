-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import           Control.Monad (filterM, mapM)
import qualified Data.Map as M
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.FilePath ((</>), takeBaseName, takeFileName)

import qualified Post as P
import qualified Template as T

-- Applies the IO-performing function f to every file in a given directory.
mapFiles :: (FilePath -> IO a) -> FilePath -> IO [a]
mapFiles f dir = enumerateFiles >>= filterM doesFileExist >>= mapM f
  -- Prepend the directory names to the names returned by getDirectoryContents.
  where enumerateFiles = fmap (fmap (dir </>)) (getDirectoryContents dir)

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
readPosts = mapFiles readPost

-- Given the post template and the global context, expands the template with the
-- context for the post.
expandPost :: T.Template -> T.Context -> P.Post -> String
expandPost tmpl ctx post = T.apply tmpl $ M.union ctx (P.context post)

main :: IO ()
main = do
  templates <- readTemplates "templates/"
  posts     <- readPosts     "posts/"

  -- Create a context that contains all of the templates, to handle includes.
  let tctx = fmap T.TemplateValue templates

  -- Create a context with the field "year" set to the current year.
  (year, month, day) <- fmap (toGregorian . utctDay) getCurrentTime
  let yctx = M.singleton "year" (T.StringValue $ show year)

  putStrLn "TODO: actually generate something"

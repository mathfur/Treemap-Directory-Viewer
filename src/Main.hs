{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude hiding (catch)
import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath
import System.IO
import Control.Applicative
import Data.List
import System.Console.CmdArgs
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LC (unpack)
import Data.Aeson hiding (json)

import Types

option :: Option
option = Option {
  output = def &= typ "FILE",
  input =  def &= typ "DIR"
} &= program "treemap-directory-viewer"
  &= summary "Treemap Directory Viewer. v0.1 (C) Mathfuru 2011"

type FileTree = Tree FilePath FileSize

-- ---------------------------------------------------------------------------
-- -- | ファイルpathのライン数を得る
-- getLineSize :: FilePath -> IO Int
-- getLineSize path = (length . lines) <$> (readFile path)
getFileSize :: String -> IO FileSize
getFileSize path =
  (bracket
    (openFile path ReadMode)
    hClose
    (\h -> fromIntegral <$> hFileSize h))
  `catch` (\(_ :: SomeException) -> return 0)

-- | topdirの子孫ディレクトリの容量付きツリーを得るためのIOを生成
getDirTree :: FilePath -> IO FileTree
getDirTree dir = getDirTree' 0 dir
  where
    getDirTree' :: Int -> FilePath -> IO FileTree
    getDirTree' depth topdir = do
         names <- getDirectoryContents topdir
         let properNames = filter (`notElem` [".", ".."]) names
         resultTree <- forM properNames $ \nm -> do
           let path = topdir </> nm
           isDirectory <- doesDirectoryExist path
           if isDirectory
             then getDirTree' (depth + 1) path
             else (Leaf <$> (return depth) <*> (return path) <*> getFileSize path)
         return $ Branch depth topdir resultTree

excludeHiddenEntry :: (FilePath -> Bool) -> FileTree -> FileTree
excludeHiddenEntry f tree = fromJust $ excludeHiddenEntry' f tree
  where
    excludeHiddenEntry' :: (FilePath -> Bool) -> FileTree -> Maybe FileTree
    excludeHiddenEntry' p' (leaf@(Leaf _ path _)) = if p' path then Just leaf else Nothing
    excludeHiddenEntry' p' (Branch depth path ts)
       | p' path = Just $ Branch depth path $ mapMaybe (excludeHiddenEntry' p') ts
       | otherwise = Nothing

haveEnableExt :: [String] -> FileTree -> FileTree
haveEnableExt f tree = fromJust $ haveEnableExt' f tree
  where
    haveEnableExt' :: [String] -> FileTree -> Maybe FileTree
    haveEnableExt' exts (leaf@(Leaf _ path _)) = if (any (`isSuffixOf` path) exts) then Just leaf else Nothing
    haveEnableExt' exts (Branch depth path ts)
      | ts == [] = Nothing
      | otherwise = Just $ Branch depth path $ mapMaybe (haveEnableExt' exts) ts

instance (ToJSON b) => ToJSON (Tree FilePath b) where
  toJSON (Leaf depth path size) = object ["depth" .= depth, "basename" .= (last $ splitPath path), "name" .= path, "size" .= size]
  toJSON (Branch depth path ts) = object ["depth" .= depth, "basename" .= (last $ splitPath path), "name" .= path, "children" .= toJSON ts]

----------------------------------------------------------------------------
main :: IO ()
main = do
  opts <- cmdArgs option
  -- let rectToDraw = Rect 0 0 2000 2000
  let outputFile = Types.output opts
  inputDir <- fromMaybe <$> getCurrentDirectory <*> (return $ Types.input opts)
  tree_only_hs <- excludeHiddenEntry (not . ("/." `isInfixOf`)) <$> getDirTree inputDir
  let json = LC.unpack $ encode $ toJSON (tree_only_hs :: FileTree)
  if (isJust outputFile) then writeFile (fromJust outputFile) $ json
                         else putStr                          $ json

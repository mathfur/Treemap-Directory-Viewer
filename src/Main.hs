{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Writer
import Types
import System.Directory
import System.Exit
import System.FilePath
import Control.Applicative
import Data.List
import Text.Regex
import Data.Char

import             Blaze.ByteString.Builder
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.ByteString.Char8 as B
import             Data.Text (Text)
import qualified   Data.Text as T
import qualified   Text.XmlHtml as X

import             Text.Templating.Heist
import Data.String
import Control.Arrow

import IO
import Maybe (fromMaybe)

-- TODO: ラベルを/以降のみにする on debugging
-- TODO: ラベル大きさを深さと連動させる on debugging
-- TODO: portrailラベルは縦に on debugging
-- TODO: ラベルのアルファ度を深さと連動させる
-- TODO: ノード以外のセルの背景色はなしにする

---------------------------------------------------------------------------
-- | ファイルpathのライン数を得る
getLineSize :: FilePath -> IO Int
getLineSize path = (length . lines) <$> (readFile path)

-- | getFileSize
getFileSize :: String -> IO (Maybe Integer)
getFileSize path = catch
  (bracket
    (openFile path ReadMode)
    hClose
    (\h -> hFileSize h >>= return . Just))
  (const $ return Nothing)

-- | path子孫のTreeを得る
getDirTree :: FilePath -> IO (Tree FilePath Int)
getDirTree topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (not .("swp" `isSuffixOf`)) $ filter (`notElem` [".", ".."]) names
  resultTree <- forM properNames $ \name -> do -- paths :: Tree FilePath Int
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getDirTree path -- :: IO (Tree FilePath Int)
      else (getFileSize path)>>=(\n -> return $ Leaf path $ fromIntegral $ fromMaybe 0 n) -- :: IO (Tree FilePath Int)
  return $ Branch topdir resultTree -- :: IO (Tree FilePath Int)
  
-- あとこれがいる
--render :: Point -> Point -> Int -> BinaryTree a -> Html
--render leftTop rightBottom depth ( BinaryTree t1 t2 ) = do
--    render (depth -1 ) t1 :: 
--    render (depth -1 ) t2
--    render me
--  where
--    v1 = volume t1
--    v2 = volume t2

----------------------------------------------------------------------------
-- | X.Elementのラッパー use for String
-- [helper function]
elementS :: String -> [(String,String)] -> [X.Node] -> X.Node
elementS tag attrs children = X.Element (T.pack tag) (map (T.pack *** T.pack) attrs) children

----------------------------------------------------------------------------
-- | RectPH 0 0 100 100 |-> [(("x","0"),("y","0"),("width","100"),("height","100"))]
rectAttrs :: RectPH -> [(String,String)]
rectAttrs RectPH{..} = [("style","fill-opacity:0.3;fill:#880000;"),("x",show x),("y",show y),("width",show (width-1)),("height",show (height-1))]

---------------------------------------------------------------------------
-- | Minimum Cell.
getAtomicCell :: Label -> RectPH -> [X.Node]
getAtomicCell label rect = [elementS "rect" (rectAttrs rect) [],adjustedText (last $ T.split (=='/') $ T.pack label) rect] 

----------------------------------------------------------------------------
-- |
adjustedText :: Text -> RectPH -> X.Node
adjustedText label RectPH{..}
  | T.length label == 0 = elementS "text" [] [X.TextNode "<>"]
  | isPortrait = elementS "text" [("style","fill-opacity:0.3;fill:#880000;"),("x",show $ x+(width `quot` 2)),("y",show y),("writing-mode","tb"),("textlength",show height),("font-size",show $ min height (width `quot` (T.length label)))] $ [X.TextNode label]
  | otherwise  = elementS "text" [("style","fill-opacity:0.3;fill:#880000;"),("x",show x),("y",show $ y+(height `quot` 2)),("textlength",show width),("font-size",show $ min height (width `quot` (T.length label)))] $ [X.TextNode label]

----------------------------------------------------------------------------
-- | get html for drawing tree on rect.
getTreeMapCell :: (HaveVolume b,Show a) => RectPH -> Tree a b -> [X.Node]
getTreeMapCell rect (Leaf a b) = getAtomicCell ((show a)::String) rect
getTreeMapCell rect (all_t@(Branch a ts)) = 
  (getAtomicCell (show a::String) rect)++( concat $ zipWith getTreeMapCell (rect `divideBy` (map volume ts)) ts )

---------------------------------------------------------------------------
-- | rectPH is divide By vs ratio.
-- if vs = [1,2,3], then rect devideBy vs is three rect.
-- second one is twice size then frist one.
-- third one is three-second size then second one.
divideBy :: RectPH -> [Int] -> [RectPH]
divideBy (rc@RectPH{..}) vs
  | isPortrait = zipWith (\y' h' -> rc{y=y',height=h',isPortrait = not isPortrait,depth = depth + 1}) ((scanl (+) y) $ scaleTo height) $ scaleTo height
  | otherwise  = zipWith (\x' w' -> rc{x=x',width=w' ,isPortrait = not isPortrait,depth = depth + 1}) ((scanl (+) x) $ scaleTo width) $ scaleTo width
  -- | scaleTo l ; vsを拡大して和がlになるようにする
  where scaleTo l = map (\h -> floor $ (fromIntegral h) / (fromIntegral $ sum vs)) $ map (*l) vs


----------------------------------------------------------------------------
main = do
  tree <- getDirTree "/home/furuta/src/haskell/TreeMapDirectoryViewer/"
  print tree
  --let tree = Branch "a" $ [Leaf "b" 1,Leaf "c" 2,Leaf "d" 3,Leaf "e" 4] :: Tree FilePath Int
  let inner_svg = getTreeMapCell (RectPH 0 0 1000 1000 False 0) tree
  let svg = elementS "svg" [("width","1000"),("height","1000")] inner_svg
  let elem = elementS "html" [("lang","ja")] [svg]
  B.writeFile "output.html" $ toByteString $ X.render $ X.HtmlDocument X.UTF8 Nothing [elem]
  

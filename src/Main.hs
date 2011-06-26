{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Writer
import Types

{-
import System.Directory

getLineSize :: FilePath -> IO Int
getLineSize path = (length . lines) <$> readFile

getFileTree :: FilePath -> IO (Tree (FilePath,Int))
getFileTree path = do
  if isDirectory path then
    paths <- getDirectoryContents path
    trees <- mapM getFileTree paths
    return $ Branch (path,size) $ trees
  else
    size <- getLineSize path
    return $ Leaf (path,size)

-- あとこれがいる
render :: Point -> Point -> Int -> BinaryTree a -> Html
render leftTop rightBottom depth ( BinaryTree t1 t2 ) = do
    render (depth -1 ) t1 :: 
    render (depth -1 ) t2
    render me
  where
    v1 = volume t1
    v2 = volume t2

-- TODO:
-- 描画時にディレクトリ名がいるが
-- data Tree型にそれが含まれていない

  
-}

{-
newtype Foo w a = Foo { runFoo :: (w,a) }

instance Monoid (Foo a) where
  (Foo (w,a)) `mappend`

instance Monad Foo where
  return x = Foo $ (mempty,a)
  (Foo (w',a')) >>= f = 
    where (w'',b) = runFoo f a
-}

import             Blaze.ByteString.Builder
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.ByteString.Char8 as B
import             Data.Text (Text)
import qualified   Data.Text as T
import qualified   Text.XmlHtml as X

import             Text.Templating.Heist
import Data.String
import Control.Arrow


---------------------------------------------------------------------------
-- | Rect with information whichever Portrait or Horizontal.
data RectPH = RectPH { x :: Int, y :: Int, width :: Int, height :: Int, isPortrait :: Bool }

----------------------------------------------------------------------------
-- | X.Elementのラッパー use for String
-- [helper function]
elementS :: String -> [(String,String)] -> [X.Node] -> X.Node
elementS tag attrs children = X.Element (T.pack tag) (map (T.pack *** T.pack) attrs) children

----------------------------------------------------------------------------
-- | RectPH 0 0 100 100 |-> [(("x","0"),("y","0"),("width","100"),("height","100"))]
rectAttrs :: RectPH -> [(String,String)]
rectAttrs RectPH{..} = [("x",show x),("y",show y),("width",show width),("height",show height)]

---------------------------------------------------------------------------
-- | Minimum Cell.
getAtomicCell :: Label -> RectPH -> X.Node
getAtomicCell label rect = elementS "rect" (rectAttrs rect) []
   -- TODO: Label to paint.

----------------------------------------------------------------------------
-- | get html for drawing tree on rect.
getTreeMapCell :: (Show a) => RectPH -> Tree a -> [X.Node]
getTreeMapCell rect (Leaf a) = [getAtomicCell ((show a)::String) rect]
getTreeMapCell rect (all_t@(Branch a ts)) = []  -- TODO: childrenのセルを描く map (\t -> percentage t all_t) ts
  where
    percentage t' ts' = floor $ ((fromIntegral $ volume t') / (fromIntegral $ volume ts'))
        -- under construntion.

----------------------------------------------------------------------------
main = do
  let tree = Leaf "foo"
  let inner_svg = getTreeMapCell (RectPH 0 0 100 100 True) tree
  let svg = elementS "svg" [("width","300"),("height","300")] inner_svg
  let elem = elementS "html" [("lang","ja")] [svg]
  B.writeFile "output.html" $ toByteString $ X.render $ X.HtmlDocument X.UTF8 Nothing [elem]


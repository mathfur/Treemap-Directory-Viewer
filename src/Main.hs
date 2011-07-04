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
import Test.QuickCheck

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


---------------------------------------------------------------------------
--getDirTree :: FilePath -> IO (Tree FilePath Int)
--getFileSize :: String -> IO (Maybe Integer)
--getLineSize :: FilePath -> IO Int
--elementS :: String -> [(String,String)] -> [X.Node] -> X.Node

prop_divideBy2 rect xs = length rect' == length xs
  where rect' = rect `divideBy` xs

-- rect divideBy one rect.
prop_divideBy1 rect0 i = and [ (centerX $ rect $ rect1) == (centerX $ rect $ rect0),
    (centerY $ rect $ rect1) == (centerY $ rect $ rect0),
    (isPortrait $ rect1) == (not $ isPortrait $ rect0),
    (depth $ rect1) == (depth $ rect0) + 1
  ]
  where rect1 = head $ rect0 `divideBy` [i]


-- xs =~ (rect `divideBy` xs)
 
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
  
----------------------------------------------------------------------------
-- | X.Elementのラッパー use for String
-- [helper function]
elementS :: String -> [(String,String)] -> [X.Node] -> X.Node
elementS tag attrs children = X.Element (T.pack tag) (map (T.pack *** T.pack) attrs) children

---------------------------------------------------------------------------
-- | Minimum Cell.
getAtomicCell :: Label -> RectPH -> [PreNode]
getAtomicCell label rect_ph = [ PreRectNode (rect rect_ph) rect_attr, PreTextNode (rect rect_ph) text_attr ]
  where 
    rect_attr = RectAttr $ Color{ a=100,r=0,g=255,b=0 }
    text_attr = TextAttr {
      fontColor = Color{a=30,r=255,g=0,b=0},
      text = label,
      fontSize = Nothing,
      isVertical = isPortrait rect_ph
    }
      
----------------------------------------------------------------------------
-- | get html for drawing tree on rect.
getTreeMapCell :: (HaveVolume b,Show a) => RectPH -> Tree a b -> [PreNode]
getTreeMapCell rect (Leaf a b) = getAtomicCell ((show a)::String) rect
getTreeMapCell rect (all_t@(Branch a ts)) = 
  (getAtomicCell (show a::String) rect) ++
  ( concat $ zipWith getTreeMapCell (rect `divideBy` (map volume ts)) ts )

---------------------------------------------------------------------------
-- | rectPH is divide By vs ratio.
-- if vs = [1,2,3], then rect devideBy vs is three rect.
-- second one is twice size then frist one.
-- third one is three-second size then second one.
divideBy :: RectPH -> [Float] -> [RectPH]
divideBy (rc@RectPH{..}) vs
  | isPortrait = zipWith (\y' h' -> rc{
          rect=Rect{centerX=centerX rect,centerY=y',width=width rect,height=h'},
          isPortrait = False,
          depth = depth + 1
        }) (ys::[Float]) $ (scaleTo (height rect) vs)
  | otherwise  = zipWith (\x' w' -> rc{
          rect = Rect{centerX=x',centerY=centerY rect,width=w',height=height rect},
          isPortrait = True,
          depth = depth + 1
        }) (xs::[Float]) $ (scaleTo (width rect) vs)
  -- | scaleTo l ; vsを拡大して和がlになるようにする
  where
    xs = scanl (+) (centerX rect) $ scaleTo (width rect) vs
    ys = scanl (+) (centerY rect) $ scaleTo (height rect) vs

scaleTo :: Float -> [Float] -> [Float]
scaleTo l as = map (/ sum as) $ map (*l) as

----------------------------------------------------------------------------
-- | generate X.Node from PreRectNode or PreTextNode.
pre2XNode :: PreNode -> X.Node
pre2XNode pre_node = case pre_node of
  PreRectNode Rect{..} rect_attr ->
    elementS "rect" attrs []
      where 
        attrs = [
          ("style","fill-opacity:" ++ (show $ floatAlpha $ color $ rect_attr )),
          ("x",show centerX),
          ("y",show centerY),
          ("width",show width),
          ("height",show height),
          ("stroke-width","2"),
          ("stroke","white"),
          ("fill",colorCode $ color $ rect_attr) 
          ]
  PreTextNode Rect{..} text_attr -> func
    where
      func
        | length (text text_attr) == 0 = elementS "text" [] [X.TextNode "<>"]
        | (isVertical text_attr) = elementS "text" [
            ("style","fill-opacity:" ++ (show $ floatAlpha $ fontColor $ text_attr ) ++ 
                     ";fill:"        ++ (colorCode $ fontColor $ text_attr)  ++ ";"),
            ("x",show $ floor $ centerX+(width / (2::Float))),
            ("y",show centerY),
            ("writing-mode","tb"),
            ("textlength",show height),
            ("font-size",show $ floor $ min height (width / (fromIntegral $ length $ text text_attr)))
          ] $ [X.TextNode $ T.pack $ text text_attr]
        | otherwise  = elementS "text" [
            ("style","fill-opacity:" ++ (show $ floatAlpha $ fontColor $ text_attr ) ++ 
                     ";fill:"        ++ (colorCode $ fontColor $ text_attr)  ++ ";"),
            ("x",show centerX),
            ("y",show $ floor $ centerY+(height / (2::Float))),
            ("textlength",show width),
            ("font-size",show $ floor $ min height (width / (fromIntegral $ length $ text text_attr)))
          ] $ [X.TextNode $ T.pack $ text text_attr]

modifyText :: PreNode -> PreNode
modifyText pre_node = case pre_node of
  PreTextNode rect (text_attr@TextAttr{..}) ->
    PreTextNode rect text_attr{ text = T.unpack $ last $ T.split (=='/') $ T.pack text }
  PreRectNode rect attr -> PreRectNode rect attr

----------------------------------------------------------------------------
main = do
  tree <- getDirTree "/home/furuta/src/haskell/TreeMapDirectoryViewer/"
  print tree
  print "---"
  --let tree = Branch "a" $ [Leaf "b" 1,Leaf "c" 2,Leaf "d" 3,Leaf "e" 4] :: Tree FilePath Int
  let pre_nodes = getTreeMapCell (RectPH (Rect 0 0 1000 1000) False 0) tree :: [PreNode]
  print pre_nodes
  print "---"
  let inner_svg = map (pre2XNode.modifyText) pre_nodes :: [X.Node]
  print inner_svg
  print "---"
  let svg = elementS "svg" [("width","1000"),("height","1000")] inner_svg
  let elem = elementS "html" [("lang","ja")] [svg]
  B.writeFile "output.html" $ toByteString $ X.render $ X.HtmlDocument X.UTF8 Nothing [elem]
  

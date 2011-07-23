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
import Text.Regex.Posix
import Data.Char
import Test.QuickCheck
import System.Console.CmdArgs

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
import Maybe
import Numeric

option =  Option {
  script = def &= argPos 0 &= typ "SCRIPT",
  inputFiles = def &= args &= typ "FILE"
} &= program "(program name)"
  &= summary "(description and version)"

---------------------------------------------------------------------------
prop_divideBy2 rect xs = length rect' == length xs
  where rect' = rect `divideBy` xs

-- rect divideBy one rect.
prop_divideBy1 rect0 i = and [ (centerX $ rect $ rect1) == (centerX $ rect $ rect0),
    (centerY $ rect $ rect1) == (centerY $ rect $ rect0),
    (isPortrait $ rect1) == (not $ isPortrait $ rect0),
    (depth $ rect1) == (depth $ rect0) + 1
  ]
  where rect1 = head $ rect0 `divideBy` [i]

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
  let properNames = filter (`notElem` [".", ".."]) names
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
    rect_attr = RectAttr $ Color{ a=255,r=0xa5,g=0xef,b=0xcc }
    text_attr = TextAttr {
      fontColor = Color{a= depth2Alpha $ depth rect_ph ,r=0x6b,g=0x9b,b=0xcc},
      text = label,
      fontSize = Nothing,
      isVertical = isPortrait rect_ph
    }

depth2Alpha :: Int -> Int
--depth2Alpha x = floor $ (255::Float) / (log (fromIntegral $ 1 + x) + 1.0)
depth2Alpha x = if (x < 0 || x > 255) then 0 else floor $ (255::Float) / (fromIntegral x)
      
---------------------------------------------------------------------------
-- | get html for drawing tree on rect.
getPreNodesFromRectAndTree :: HaveVolume b => RectPH -> Tree String b -> [PreNode]
getPreNodesFromRectAndTree rect (Leaf a b) = getAtomicCell (a++"("++ show (floor (volume b / 1000.0)) ++")") rect
getPreNodesFromRectAndTree rect (all_t@(Branch a ts)) = 
  (getAtomicCell (a++"("++ show(floor $ volume all_t / 1000.0) ++")") rect) ++
  ( concat $ zipWith getPreNodesFromRectAndTree (rect `divideBy` (map volume ts)) ts )

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
          --("style","fill-opacity:" ++ (show $ floatAlpha $ color $ rect_attr )),
          ("x",show centerX),
          ("y",show centerY),
          ("width",show width),
          ("height",show height),
          ("stroke-width","1"),
          ("stroke","white"),
          ("fill",colorCode $ color $ rect_attr) 
          ]
  PreTextNode Rect{..} text_attr -> func
    where
      func
        | length (text text_attr) == 0 = elementS "text" [] [X.TextNode "<>"]
        | (isVertical text_attr) = elementS "text" [
            ("style","fill:" ++ (colorCode $ fontColor $ text_attr)  ++ ";"),
            ("opacity",showFFloat (Just 3) (floatAlpha $ fontColor $ text_attr ) ""),
            ("x",show $ floor $ centerX+(width / (2::Float))),
            ("y",show centerY),
            ("writing-mode","tb"),
            --("textlength",show height),
            ("font-size",show $ floor $ min width (height / (fromIntegral $ length $ text text_attr)))
          ] $ [X.TextNode $ T.pack $ text text_attr]
        | otherwise  = elementS "text" [
            ("style","fill:" ++ (colorCode $ fontColor $ text_attr)  ++ ";"),
            ("opacity",showFFloat (Just 3) (floatAlpha $ fontColor $ text_attr ) ""),
            ("x",show centerX),
            ("y",show $ floor $ centerY+(height / (2::Float))),
            --("textlength",show width),
            ("font-size",show $ floor $ min height (width / (fromIntegral $ length $ text text_attr)))
          ] $ [X.TextNode $ T.pack $ text text_attr]

modifyText :: PreNode -> PreNode
modifyText pre_node = case pre_node of
  PreTextNode rect (text_attr@TextAttr{..}) ->
    PreTextNode rect text_attr{ text = T.unpack $ last $ T.split (=='/') $ T.pack text }
  PreRectNode rect attr -> PreRectNode rect attr

withVisibleExtension :: [String] -> FilePath -> Bool
withVisibleExtension exts path = (or $ map (`isSuffixOf` path) exts)

excludeHiddenEntry :: (FilePath -> Bool) -> Tree FilePath Int -> Maybe ( Tree FilePath Int )
excludeHiddenEntry p (leaf@(Leaf path n)) = if p path then Just leaf else Nothing
excludeHiddenEntry p (Branch path ts)
  | p path = Just $ Branch path $ mapMaybe (excludeHiddenEntry p) ts
  | otherwise = Nothing

includeEnableExtensions :: [String] -> Tree FilePath Int -> Maybe ( Tree FilePath Int )
includeEnableExtensions exts (leaf@(Leaf path n)) = if (withVisibleExtension exts path) then Just leaf else Nothing
includeEnableExtensions exts (Branch path ts)
  | ts == [] = Nothing
  | otherwise = Just $ Branch path $ mapMaybe (includeEnableExtensions exts) ts

writeIntoHtml :: FilePath -> Rect -> [X.Node] -> IO ()
writeIntoHtml path Rect{..} inner = do
  let svg = elementS "svg" [("width",show width),("height",show height)] inner
  let elem = elementS "html" [("lang","ja")] [svg]
  B.writeFile path $ toByteString $ X.render $ X.HtmlDocument X.UTF8 Nothing [elem]

----------------------------------------------------------------------------
main = do
  opts <- cmdArgs option
  let rectToDraw = Rect 0 0 2000 2000
  tree_only_hs <- ((getDirTree "/home/furuta/src/haskell/language-python/src/Language/Python/") >>= (return.fromJust.excludeHiddenEntry (not .("/." `isInfixOf`))) >>= (return.fromJust.includeEnableExtensions ["hs","lhs"] ))
  let pre_nodes = getPreNodesFromRectAndTree (RectPH rectToDraw False 0) tree_only_hs  :: [PreNode]
  let inner_svg = sortBy (\e f -> if X.tagName e == Just "rect" then LT else GT) $ map (pre2XNode.modifyText) pre_nodes :: [X.Node]
  writeIntoHtml "output.html" rectToDraw inner_svg
  

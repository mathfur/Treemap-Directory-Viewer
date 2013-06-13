{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Types where

import Control.Arrow
import Numeric
import Data.Data

data Tree a b = Leaf Int a b | Branch Int a [Tree a b] deriving (Eq,Show)

class HaveVolume a where
  volume :: a -> Float

instance HaveVolume b => HaveVolume (Tree a b) where
  volume (Leaf _ _ b) = volume b
  volume (Branch _ _ ts) = sum $ map volume ts

instance (Eq a,Eq b,HaveVolume b) => Ord (Tree a b) where
  t1 <= t2 = (volume t1 <= volume t2)

instance HaveVolume Float where
  volume = id

instance HaveVolume Int where
  volume = fromIntegral 

type Label = String

type Html = String

type FileSize = Int

-- | Rect with information whichever Portrait or Horizontal.
data RectPH = RectPH {
  rect :: Rect,
  isPortrait :: Bool,
  depth :: Int
} deriving (Eq,Show)

data Color = Color { r :: Int, g :: Int, b :: Int, a :: Int } deriving (Eq,Show)

colorCode :: Color -> String
colorCode Color{..} = "#" ++ toBeTwoChar r ++ toBeTwoChar g ++ toBeTwoChar b
  where toBeTwoChar n = (if n < 16 then "0" else "") ++ showHex n ""

floatAlpha :: Color -> Float
floatAlpha color = (fromIntegral $ a color) / 255.0

data PreNode = PreRectNode Rect RectAttr | PreTextNode Rect TextAttr deriving (Eq,Show)

forget :: PreNode -> Rect
forget pre_node = case pre_node of
  PreRectNode rect _ -> rect
  PreTextNode rect _ -> rect

data RectAttr = RectAttr {
  color :: Color
} deriving (Eq,Show)

data TextAttr = TextAttr {
  fontColor :: Color,
  text :: String,
  fontSize :: Maybe Float,
  isVertical :: Bool
} deriving (Eq,Show)
 
data Rect = Rect {
  centerX :: Float,
  centerY :: Float,
  width :: Float,
  height :: Float
} deriving (Eq,Show)

left :: Rect -> Float
left = uncurry (-) . (centerX &&& ((/2).width))

top :: Rect -> Float
top = uncurry (-) . (centerY &&& ((/2).width))

right :: Rect -> Float
right = uncurry (+) . (centerX &&& ((/2).width))

bottom :: Rect -> Float
bottom = uncurry (+) . (centerY &&& ((/2).width))
  
----------------------------------------------------------------------

data Option  = Option {
  output :: Maybe String,
  input :: Maybe String
} deriving (Data,Show,Typeable)


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Test.QuickCheck
import Control.Applicative
import Control.Arrow
import Numeric
import Data.Data

data Tree a b = Leaf a b | Branch a [Tree a b] deriving (Eq,Show)

class HaveVolume a where
  volume :: a -> Float

instance HaveVolume b => HaveVolume (Tree a b) where
  volume (Leaf a b) = volume b
  volume (Branch s ts) = sum $ map volume ts

instance (Eq a,Eq b,HaveVolume b) => Ord (Tree a b) where
  t1 <= t2 = (volume t1 <= volume t2)

instance HaveVolume Float where
  volume = id

instance HaveVolume Int where
  volume = fromIntegral 

type Label = String

type Html = String

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
instance Arbitrary RectPH where
  arbitrary = RectPH <$> 
      (arbitrary :: Gen Rect) <*>
      (arbitrary :: Gen Bool) <*>
      (arbitrary :: Gen Int)

instance Arbitrary Rect where
  arbitrary = Rect <$> 
      (arbitrary :: Gen Float) <*>
      (arbitrary :: Gen Float) <*>
      (arbitrary :: Gen Float) <*>
      (arbitrary :: Gen Float)

instance (Arbitrary a,Arbitrary b) => Arbitrary (Tree a b) where
  arbitrary = oneof [
      Leaf <$> arbitrary <*> arbitrary,
      Branch <$> arbitrary <*> listOf arbitrary
    ]


data Option  = Option {
  script :: String,
  inputFiles :: [String]
} deriving (Data,Show,Typeable)


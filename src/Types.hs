module Types where

import Test.QuickCheck
import Control.Applicative

data Tree a b = Leaf a b | Branch a [Tree a b] deriving (Eq,Show)

class HaveVolume a where
  volume :: a -> Int

instance HaveVolume b => HaveVolume (Tree a b) where
  volume (Leaf a b) = volume b
  volume (Branch s ts) = sum $ map volume ts

instance (Eq a,Eq b,HaveVolume b) => Ord (Tree a b) where
  t1 <= t2 = (volume t1 <= volume t2)

instance HaveVolume Int where
  volume = id

type Label = String

type Html = String

-- | Rect with information whichever Portrait or Horizontal.
data RectPH = RectPH {
  x :: Int,
  y :: Int,
  width :: Int,
  height :: Int,
  isPortrait :: Bool,
  depth :: Int
} deriving (Eq,Show)

----------------------------------------------------------------------
instance Arbitrary RectPH where
  arbitrary = RectPH <$> 
      (arbitrary :: Gen Int) <*>
      (arbitrary :: Gen Int) <*>
      (arbitrary :: Gen Int) <*>
      (arbitrary :: Gen Int) <*>
      (arbitrary :: Gen Bool) <*>
      (arbitrary :: Gen Int)

instance (Arbitrary a,Arbitrary b) => Arbitrary (Tree a b) where
  arbitrary = oneof [
      Leaf <$> arbitrary <*> arbitrary,
      Branch <$> arbitrary <*> listOf arbitrary
    ]

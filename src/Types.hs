module Types where

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
}

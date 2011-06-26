module Types where

data Tree a = Leaf a | Branch a [Tree a] deriving (Eq,Show)
data BinaryTree a = BinaryLeaf a | BinaryBranch (BinaryTree a) (BinaryTree a) deriving (Eq,Show)

class HaveVolume a where
  volume :: a -> Int

instance HaveVolume a => HaveVolume (Tree a) where
  volume (Leaf a) = volume a
  volume (Branch a ts) = sum $ map volume ts

instance HaveVolume a => HaveVolume (BinaryTree a) where
  volume (BinaryLeaf a) = volume a
  volume (BinaryBranch t1 t2) = volume t1 + volume t2

instance (Eq a,HaveVolume a) => Ord (Tree a) where
  t1 <= t2 = (volume t1 <= volume t2)

type Label = String

type Html = String

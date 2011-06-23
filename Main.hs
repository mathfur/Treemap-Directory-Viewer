import System.Directory

data Tree a = Leaf a | Branch [Tree a]
data BinaryTree a = BinaryLeaf a | BinaryBranch (BinaryTree a) (BinaryTree a)

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

class haveVolume a where
  volume :: a -> Int

instance haveVolume (FilePath,Int) where
  volume = snd

instance haveVolume a => haveVolume (Tree a) where
  volume (Leaf a) = volume a
  volume (Brance ts) = sum $ map volume ts

instance haveVolume a => haveVolume (BinaryTree a) where
  volume (BinaryLeaf a) = volume a
  volume (BinaryBranch t1 t2) = volume t1 + volume t2

instance Ord (Tree a) where
  t1 <= t2 = volume t1 <= volume t2

cutSameSize :: (Ord a,haveVolume a) => [a] -> ([a],[a])
cutSameSize [] = ([],[])
cutSameSize x:xs = if l1 < l2 then ((x:ys1),ys2) else (ys1,x:ys2)
  where
    ys1 = fst $ cutSameSize xs --:: [a]
    ys2 = snd $ cutSameSize xs --:: [a]
    l1 = volume ys1 -- :: a
    l2 = volume ys2 -- :: a

binarize :: (haveVolume a) => Tree a -> BinaryTree a
binarize (Leaf a) = BinaryLeaf a
binarize (Branch ls) 
  | length ls >= 2 = BinaryBranch (binarize fstCutTree) (binarize sndCutTree)
  | length ls == 1 = binarize sndCutTree
    where
      cutTree = cutSameSize ls -- :: ([Tree a],[Tree a])
      fstCutTree = Branch $ fst cutTree -- :: Tree a
      sndCutTree = Branch $ snd cutTree -- :: Tree a
    
-- いまここまでできている
-- getFileTree >>= (return.binarize) :: FilePath -> IO (BinaryTree (FilePath,Int))

data PaintArea = PaintArea Color Label Point Point
type Label = String

type Html = String

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

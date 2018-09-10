-- Written by: Shania Daley && Sharell Scott 

import Test.QuickCheck
import Control.Monad( liftM, liftM2)
import Data.Char

------------------------------------------------------------------------
-- Binary trees
data BTree = Empty | Branch Char BTree BTree
            deriving (Show,Eq)

-- Multiway trees
data MTree = Node Char [MTree]
        deriving (Eq, Show)

------------------------------------------------------------------------
-- Examples 

t1 = Branch 'x' 
       (Branch 't' 
         (Branch 'a' Empty Empty) 
         Empty) 
       (Branch 'w' 
         (Branch 'm' Empty Empty) 
         (Branch 'q' Empty Empty))

t2 = Node 'u' 
       [Node 'c' [],
        Node 'q' [],
        Node 'n' 
          [Node 'm' [],
           Node 'g' [],
           Node 'j' []],
        Node 'y' 
          [Node 'z' []]]


-- Counting BTree Branch nodes
bcount :: BTree -> Int
bcount Empty = 0
bcount (Branch _ tl tr) = 1 + bcount tl + bcount tr

-- Counting MTree Nodes
mcount :: MTree -> Int
mcount (Node _ ts) = 1 + sum (map mcount ts)

--  preorder traversal for BTrees
preorder Empty = ""
preorder (Branch c tl tr) = c:(preorder tl ++ preorder tr)

-- inorder traversal for BTrees
inorder Empty = ""
inorder (Branch c tl tr) = inorder tl ++ [c] ++ inorder tr
------------------------------------------------------------------------
------------------------------------------------------------------------
fix = error "Please fix me!"

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Part I: Binary and Multiway Trees

------------------------------------------------------------------------
-- Problem 1: BTree depth

bmaxDepth Empty =  -1
bmaxDepth (Branch _ Empty Empty) = 0
bmaxDepth (Branch _ tl tr) = 1 + maximum [bmaxDepth tl + bmaxDepth tr]

------------------------------------------------------------------------
-- Problem 2: 
mmaxDepth (Node _ []) = 0
mmaxDepth (Node _ xs) = 1 + (maximum (map mmaxDepth xs)) 

------------------------------------------------------------------------
-- Problem 3: Collecting BTree leaves
bleaves :: BTree -> String
bleaves Empty = ""
bleaves (Branch a Empty Empty) = [a]
bleaves (Branch _ tl tr) = bleaves tl ++ bleaves tr

------------------------------------------------------------------------
-- Problem 4: Collecting MTree leaves
mleaves :: MTree -> String
mleaves (Node a []) = [a]
mleaves (Node a xs) = foldr (++)  "" (map mleaves xs)

------------------------------------------------------------------------
-- Problem 5: BTree levels 
blevel :: Int -> BTree -> String
blevel 0 (Branch _ tl tr) = ""
blevel 1 (Branch a _ _) = [a]
blevel n Empty = ""
blevel n (Branch a tl tr) = blevel (n-1) tl ++ blevel (n-1) tr



------------------------------------------------------------------------
-- Problem 6: MTree levels 
mlevel :: Int -> MTree -> String
mlevel 1 (Node a xs) = [a]
mlevel n (Node a xs) = concatMap (mlevel (n-1)) xs



------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------

t10 = Branch 'm' 
     (Branch 'h' 
      (Branch 'c' 
       (Branch 'a' Empty Empty) 
       (Branch 'e' Empty (Branch 'f' Empty Empty)))
      Empty)
     (Branch 'u' 
      (Branch 's' (Branch 'p' Empty Empty) Empty)
      (Branch 'z' Empty Empty))
     
-- QuickCheck BTree generator
instance Arbitrary BTree where
    arbitrary = sized tree
        where
          tree 0 = return Empty
          tree n = do c  <- elements (['a'..'z']++['A'..'Z'])
                      m1 <- elements [0..(2*n `div` 3)]
                      m2 <- elements [0..(2*n `div` 3)]
                      liftM2 (Branch c) (variant 1 (tree m1))
                                        (variant 2 (tree m2))

-- QuickCheck MTree generator
instance Arbitrary MTree where
    arbitrary = sized tree
        where
          tree n = do c <- elements (['a'..'z']++['A'..'Z'])
                      m <- elements [0..n]
                      liftM (Node c) (liftM (take m)
                                           (listOf (tree (2*(n-m)`div` 3))))


------------------------------------------------------------------------
-- Testing for bleaves

bleaves_prop t = and $ zipWith (==) (bleaves (bleafRel t)) ['a'..maxBound]

bleafRel :: BTree -> BTree
bleafRel t = fst $ relab (t,['a'..maxBound])
    where 
      relab (Empty,cs) = (Empty,cs)
      relab (Branch _ Empty Empty,c:cs)=(Branch c Empty Empty,cs)
      relab (Branch c tl tr,cs) = (Branch c tl' tr',cs'')
          where (tl',cs') = relab (tl,cs)
                (tr',cs'') = relab (tr,cs')


------------------------------------------------------------------------
-- Testing for mleaves

mleaves_prop t = and $ zipWith (==) (mleaves (mleafRel t)) ['a'..maxBound]

mleafRel :: MTree -> MTree
mleafRel t = fst $ relab (t,['a'..maxBound])
    where 
      relab (Node _ [],c:cs)=(Node c [],cs)
      relab (Node c ts,cs)  = foo (ts,[],cs)
          where foo ([],ts',cs) = (Node c (reverse ts'),cs)
                foo (t:ts,ts',cs) = let (t',cs') = relab (t,cs)
                                    in foo (ts,t':ts',cs')


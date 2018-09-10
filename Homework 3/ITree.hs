-- Part 2
-- Shania Daley
-- Lianna Morelli
-- Version 1.2  Sat Feb  3 20:22:11 EST 2018

module ITree where
import Data.List
import Data.Char (isAlpha)
import Test.QuickCheck    
import System.Process
import System.IO

-- This is roughly based on the binary search tree example in:
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures
pleaseFix str = error (str ++ ", Please fix!")


data ITree = Empty | Node Char ITree ITree Int deriving (Show)

singleton :: Char -> ITree
singleton c = Node c Empty Empty 0

-- Problem 5 
treeInsert :: ITree -> Char -> ITree
treeInsert Empty c = singleton c
treeInsert (Node d tl tr n) c
    | c==d      = error (c:" already in the tree")
    | c<d       = Node d (treeInsert tl c) tr (n+1)
    | otherwise = Node d tl (treeInsert tr c) n
 

-- Problem 6
-- Index is the nodes to the left of N 
index :: ITree -> Char -> Int
index Empty c = error (c:" not in the tree")                                    
index (Node d tl tr n) c
      | d == c = n
      | c < d = index tl c
      | otherwise = (index tr c) + 1 + n


-- Problem 7
-- return index i in the tree
-- d depth
-- n 
fetch :: ITree -> Int -> Char
fetch Empty n = error "index out of bounds"
fetch (Node d tl tr n) i
      | index (Node d tl tr n) d == i   = d
      | index (Node d tl tr n) d > i    = fetch tl i
      | otherwise                       = fetch tr (i-(n+1))

-- Problem 8
-- (rr t) does a right rotation as in Figure 2 in the writeup
-- (rl t) does a left rotation as in Figure 2 in the writeup
rr, rl :: ITree -> ITree
rr (Node q (Node p a b j) c k) = Node p a (Node q b c (k - j - 1)) j
rl (Node p a (Node q b c k) j) = Node q (Node p a b j) c (k + j + 1)
                     
reroot :: ITree -> Char -> ITree
reroot Empty c = error (c:" is not in the ITree")
reroot nd@(Node d tl tr n) c
                     | d == c = nd
                     | d > c = rr (Node d (reroot tl c) tr n)
                     | otherwise = rl (Node d tl (reroot tr c) n) 
      
------------------------------------------------------------------------
-- Testing tools
------------------------------------------------------------------------

-- Build a ITree from a String of characters
build :: String -> ITree
build = (foldl treeInsert Empty) . nub . (filter isAlpha)


-- Made an inorder list of the ITree labels 
flatten :: ITree -> String
flatten Empty            = []
flatten (Node c tl tr _) = flatten tl ++ (c:flatten tr)

-- Tests if a list is strictly increasing, i.e.,
-- given [x0,x1,...,xn], it returns True iff x0<x1< ... < xn.
isIncreasing :: (Ord a) => [a] -> Bool
isIncreasing xs = and $ zipWith (<) xs  $ tail xs

-- Test if an ITree has the binary search tree property
isBST :: ITree -> Bool
isBST = isIncreasing . flatten

-- checkLcounts checks that the left counts in an ITree are correct
-- If they are correct, the number of elements in the tree are returned
-- If incorrect, an error is reported.
checkLcounts :: ITree -> Int
checkLcounts Empty = 0
checkLcounts t@(Node c tl tr n)
    | n/=nl     = error ("The "++( c:"-node has a left count of ")++show n
                         ++ " but the correct value is " ++show nl
                         ++ " in the ITree:\n" ++ (show t)
                        )                        
    | otherwise = nl+nr+1
    where nl = checkLcounts tl
          nr = checkLcounts tr
                  
-- Test that treeInsert results in a binary search tree
prop_treeInsert1 :: String -> Bool
prop_treeInsert1 cs = (flatten t)==sort cs'
    where cs' = nub (filter isAlpha cs)
          t   = build cs'

-- Test that treeInsert results in an ITree with correct left counts
prop_treeInsert2 :: String -> Bool
prop_treeInsert2 cs = (checkLcounts (build cs)) >= 0
                      
prop_index :: String -> Bool
prop_index str = [0..(length str')-1] == (map (index t) str'')
    where str'  = nub (filter isAlpha str)
          str'' = sort str'
          t     = build str'

prop_fetch :: String -> Bool
prop_fetch str = str'' == (map (fetch t) [0..(length str')-1])
    where str'  = nub (filter isAlpha str)
          str'' = sort str'
          t     = build str'
                  
prop_reroot str = (c=='X') && (isBST t) && (checkLcounts t >= 0)                  
    where str'  = nub ((filter isAlpha str)++"X")
          t@(Node c _ _ _) = reroot (build str') 'X'



               

------------------------------------------------------------------------
-- Display ITrees via GraphViz
--   See: http://www.graphviz.org
------------------------------------------------------------------------

------------------------------------------------------------------------
-- dump str 
--   writes the temp file /tmp/graph.gv with contents str & then opens it

dump str = do withFile "/tmp/graph.gv" WriteMode (`hPutStrLn` str)
              system "open /tmp/graph.gv"

------------------------------------------------------------------------
-- toGv t
--   translates the ITree t into a GraphViz rep and displays it

toGv t = dump ("digraph g {\n   rankdir=TB\n" ++ gvbuild t 1 ++ "}\n")
                          
xshow i = 'x':show i
          
gvbuild :: ITree -> Int -> String
gvbuild Empty i 
    = concat ["   ", xshow i, " [style=invis];\n"]
gvbuild (Node c Empty Empty p) i
    = showNode c p i
gvbuild (Node c Empty tr p) i
    = showNode c p i
      ++ concat ["   ", xshow i, " -> ", xshow (2*i),   "[style=invis];\n",
                 "   ", xshow i, " -> ", xshow (2*i+1), ";\n",
                 gvbuild Empty (2*i),  gvbuild tr (2*i+1)]
gvbuild (Node c tl Empty p) i
    = showNode c p i
      ++ concat ["   ", xshow i, " -> ", xshow (2*i),   ";\n",
                 "   ", xshow i, " -> ", xshow (2*i+1), "[style=invis];\n",
                 gvbuild tl (2*i),  gvbuild Empty (2*i+1)]
gvbuild (Node c tl tr p) i
    = showNode c p i
      ++ concat ["   ", xshow i, " -> ", xshow (2*i),   ";\n",
                 "   ", xshow i, " -> ", xshow (2*i+1), ";\n",
                 gvbuild tl (2*i),  gvbuild tr (2*i+1)]

showNode c p i
    = concat ["   ", xshow i, " [label=","\"",c:" ; ", show p, "\"","];\n"]
               
------------------------------------------------------------------------------

--Tests

t0 = Node 'd' (Node 'a' Empty (Node 'b' Empty (Node 'c' Empty Empty 0) 0) 0) (Node 'e' Empty (Node 'f' Empty Empty 0) 0) 3

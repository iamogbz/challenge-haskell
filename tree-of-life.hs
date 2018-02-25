import Data.List (intercalate)
import qualified Data.Map as Map

-- test tree to del
tree = (Branch 0 (Branch 1 (Leaf 0) (Branch 0 (Leaf 0) (Leaf 0))) (Branch 0 (Leaf 1) (Branch 1 (Leaf 0) (Leaf 1)))) 

-- binary tree data structure
data BTree = Leaf Int | Branch Int BTree BTree deriving Eq
-- custom display for tree structure
instance Show BTree where show tree = case tree of
                                        Leaf n -> show' n
                                        Branch n a b -> "(" ++ (intercalate " " [show a,show' n,show b]) ++ ")"
                                        where
                                            show' 0 = "."
                                            show' _ = "X"

-- generate list of results at each step
simulate n r t = take n (sim rmap t)
    where sim r t = t : sim rmap (apply rmap 0 t)
          -- map all possible value to the rule result
          rmap = Map.fromList [(pad 4 $ toBase 2 x, r !! x) | x <- [0..15]]

-- apply rule to complete BTree
-- rmap = all possible rule results [0000...1111]
-- root = node value of tree root
-- tree = tree sprouted from node
apply :: Map.Map [Int] Int -> Int -> BTree -> BTree
apply rmap root (Leaf n) = Leaf $ rmap Map.! [0,n,0,root]
apply rmap root (Branch n a b) = Branch (rmap Map.! [node b,n,node a,root]) (apply rmap n a) (apply rmap n b)
                        where node (Leaf x) = x
                              node (Branch x _ _) = x

-- append 0s to end of list
pad l xs = xs ++ replicate (l - length xs) 0

-- binary representation of rule
-- rule 6 => [0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
rule :: Int -> [Int]
rule = pad 16 . toBase 2

-- convert from base * to base 10
-- order input list from least significant to most
-- e.g. [0,1,1] => 6
fromBase :: (Enum a, Floating a, RealFrac a) => a -> [a] -> Int
fromBase b xs = foldr (+) 0 $ zipWith (\x y -> fromIntegral $ round (x * (b**y))) xs [0..]

-- convert to base * from base 10
-- result is ordered from least significant to most
-- e.g. 6 => [0,1,1]
toBase :: Int -> Int -> [Int]
toBase _ 0 = []
toBase b n = (mod n b):toBase b (div n b)

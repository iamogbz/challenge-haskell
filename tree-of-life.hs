import Data.List (intercalate)

-- test tree to del
tree = (Branch 0 (Branch 1 (Leaf 0) (Branch 0 (Leaf 0) (Leaf 0))) (Branch 0 (Leaf 1) (Branch 1 (Leaf 0) (Leaf 1)))) 

-- binary tree data structure
data BTree = Leaf Int | Branch Int BTree BTree deriving Eq
-- custom display for tree structure
instance Show BTree where show (Leaf n) = show' n
                          show (Branch n a b) = "(" ++ (intercalate " " [show a,show' n,show b]) ++ ")"
-- show helper
show' 0 = "."
show' _ = "X"

-- generate list of results at each step
simulate 0 _ _ = []
simulate n r t = tn : simulate (n-1) r tn where tn = apply r 0 t

-- apply rule to complete BTree
-- r = rule binary representation
-- root = node value of tree root
-- tree = tree sprouted from node
apply :: [Int] -> Int -> BTree -> BTree
apply r root (Leaf n) = Leaf $ update r [root,0,n,0]
apply r root (Branch n a b) = Branch (update r [root,node a,n,node b]) (apply r n a) (apply r n b)
                        where node (Leaf x) = x
                              node (Branch x _ _) = x

-- update a cell
-- r = rule binary representation
-- c = cell - [root, node, left, right]
update :: [Int] -> [Int] -> Int
update r c = r !! fromBase 2 (reverse $ map fromIntegral c)

-- binary representation of rule
-- rule 6 => [0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
rule :: Int -> [Int]
rule = pad 16 . toBase 2
    where pad l xs = xs ++ replicate (l - length xs) 0

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

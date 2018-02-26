import Data.List (intercalate)
import qualified Data.Map as Map

main = do
    a0 <- getLine
    let r = rule $ (read::String->Int) a0
    a1 <- getLine
    let t = (read::String->BTree) a1
    let sims = (simulate r t !!)
    a2 <- getLine
    let n = (read::String->Int) a2
    a3 <- getContents
    let an = tail $ scanl (\(a,_) (x:y:[]) -> (a + read x, tail $ init y)) (0,"") $ map words $ lines a3
    putStrLn $ unlines $ [show $ Leaf $ node $ navigate (sims i) p | (i,p) <- an]

-- binary tree data structure
data BTree = Empty | Leaf Int | Branch Int BTree BTree deriving Eq
-- custom display for tree structure
instance Show BTree where show tree = case tree of
                                        Empty -> "()"
                                        Leaf n -> show' n
                                        Branch n a b -> "(" ++ (intercalate " " [show a,show' n,show b]) ++ ")"
                                        where
                                            show' 0 = "."
                                            show' _ = "X"
-- custom read for binary tree struct
instance Read BTree where readsPrec _ = (:[]) . parse seed
-- empty node
seed = Branch (-1) Empty Empty
-- parse tree
parse (Branch _ left Empty) "" = (left,"")
parse t "" = (t,"")
parse t@(Branch n left right) (x:xs) = case x of
    '(' -> let (tree, rem) = parse seed xs in 
        case left of
            Empty -> parse (Branch n tree right) rem
            _ -> parse (Branch n left tree) rem
    ')' -> (Branch n left right, xs)
    ' ' -> parse t xs
    _ -> case t of
        Branch _ Empty _ -> parse (Branch n (Leaf $ parse' x) right) xs
        Branch (-1) _ _ -> parse (Branch (parse' x) left right) xs
        Branch _ _ Empty -> parse (Branch n left (Leaf $ parse' x)) xs
        where
            -- parse leaf
            parse' 'X' = 1
            parse' '.' = 0
            parse' ch = -1

-- navigate binary tree using path as string "<>>"
-- return value of node at destination
navigate :: BTree -> String -> BTree
navigate tree "" = tree
navigate (Leaf n) xs = Leaf 0
navigate (Branch n a b) (x:xs) | x == '<' = navigate a xs
                               | x == '>' = navigate b xs

-- generate list of results at each step
simulate r t = sim t
    where sim t = t : sim (apply rmap 0 t)
          -- map all possible value to the rule result
          rmap = Map.fromList [(pad 4 $ toBase 2 x, r !! x) | x <- [0..15]]

-- apply rule to complete BTree
-- rmap = all possible rule results [0000...1111]
-- root = node value of tree root
-- tree = tree sprouted from node
apply :: Map.Map [Int] Int -> Int -> BTree -> BTree
apply rmap root (Leaf n) = Leaf $ rmap Map.! [0,n,0,root]
apply rmap root (Branch n a b) = Branch (rmap Map.! [node b,n,node a,root]) (apply rmap n a) (apply rmap n b)

-- get tree node value
node :: BTree -> Int
node (Leaf x) = x
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

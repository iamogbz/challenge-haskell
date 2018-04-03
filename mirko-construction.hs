import Data.List
import Control.Monad

main = do
    ns <- getLine
    let s:q:_ = map (read::String->Int) $ words ns
    ns <- getLine
    let hs = map (read::String->Int) $ words ns
    ns <- getLine
    let gs = map (read::String->Int) $ words ns
    let xs = simulate $ prep hs gs
    forM_ [1..q] $ \_ -> do
        n <- getLine
        print $ (xs!!) $ (read::String->Int) n
    
-- triple where values are (height, growth, index)
type TRPL = (Int, Int, Int)

prep :: [Int] -> [Int] -> [TRPL]
prep a b = sortBy order $ zip3 a b [1..]

simulate :: [TRPL] -> [Int]
simulate [] = []
simulate xs@((_, _, i):_) = i:simulate nxs
    where
        nxs = prune $ sortBy order $ next xs
        next xs = [(a + b, b, i) | (a, b, i) <- xs]

prune :: [TRPL] -> [TRPL]
prune [] = []
prune (x:xs) = x:prune' x xs
    where
        prune' _ [] = []
        prune' (x@(a, b, i)) (y@(c, d, j):ys) = if b < d
                                                then y:prune' x ys
                                                else prune' x ys

order :: TRPL -> TRPL -> Ordering
order (a, b, i) (c, d, j) =  if a == c 
                             then compare j i
                             else compare c a
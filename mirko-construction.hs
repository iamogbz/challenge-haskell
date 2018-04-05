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
        nxs = prune $ next xs
        next xs = [(a + m, m, i) | (a, m, i) <- xs]

prune :: [TRPL] -> [TRPL]
prune = prune' [] . reverse
    where
        prune' s [] = s
        prune' s [x] = x:s
        prune' s (x@(a, _, i):y@(b, _, j):zs) = if a > b || a == b && i > j
                                                then prune' s (x:zs)
                                                else prune' (x:s) (y:zs)

order :: TRPL -> TRPL -> Ordering
order (a, _, i) (b, _, j) = if a == b
                            then compare j i
                            else compare b a

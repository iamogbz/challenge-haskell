import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Monad

main = do
    (s:q:_) <- map (read::String->Int) . words <$> getLine
    hs <- map (read::String->Int) . words <$> getLine
    gs <- map (read::String->Int) . words <$> getLine
    qs <- map (read::String->Int) . lines <$> getContents
    let queried = simulate (unique qs) (prep hs gs) IM.empty
    putStr . unlines $ map (show . (queried IM.!)) qs

-- triple where values are (growth, height, index)
data Building = B {g::Int, h::Int, k::Int}

cmp :: Building -> Building -> Ordering
cmp (B m a i) (B n b j) = compare (n, b, j) (m, a, i)

check :: [Building] -> [Building]
check (p1:ps1@(p2:ps2))
            | g p1 == g p2 = check ps1
            | otherwise =  p1 : check' (check ps1) where
                check' pp@(p2:ps2@(p3:ps3))
                    | (h p1 - h p2) * (g p3 - g p2) > (h p2 - h p3) * (g p2 - g p1) = check' ps2
                    | otherwise = pp
                check' ps = ps
check ps = ps

prep :: [Int] -> [Int] -> [Building]
prep hs gs = sortBy cmp $ zipWith3 B gs hs [1..]

-- remove duplicates, order is not preserved
unique :: Ord a => [a] -> [a]
unique = map head . group . sort

simulate :: [Int] -> [Building] -> IntMap Int -> IntMap Int
simulate [] _ rs = rs
simulate (q:qs) p@[B _ _ i] rs = simulate qs p (IM.insert q i rs) 
simulate qq@(q:qs) pp@(B m a i:ps@(B n b j:_)) rs
    | aq < bq = simulate qq ps rs
    | otherwise = simulate qs (last pp1:pp2) (IM.insert q (maximum $ map k pp1) rs)
    where
        aq = a + m * q
        bq = b + n * q
        (pp1, pp2) = span (\(B o c _) -> aq == c + o * q) pp

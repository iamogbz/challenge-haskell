import Data.List

main :: IO ()
main = interact $ unlines . map (show . decibinary . readInt) . tail . lines

-- get decibinary value at a given index
decibinary :: Int -> Int
decibinary = ((concat $ map genDecibinary [0..]) !!)
-- TODO!! OPTIMISE THIS BOTTLENECK!!
-- generate all valid decibinary values of decimal number
genDecibinary :: Int -> [Int]
genDecibinary = sort . map joinDigits . bfs [] (next []) . replicate 1 . toBase 2
    where next _ (_:[]) = []
          next seen (n:m:xs) = [seen++[n-x, m + x*2]++xs | x <- [1..n], m+x*2 < 10] ++ next (seen++[n]) (m:xs)

-- breadth first search function (with pruning of duplicates)
bfs :: (Eq a, Ord a) => [a] -> (a -> [a]) -> [a] -> [a]
bfs _ _ [] = []
bfs s f x = x ++ bfs seen f xs
    where seen = x ++ s
          xs = unique $ filter (\a -> notElem a seen) (concatMap f x)
          unique = map head . group . sort

-- convert from base * to base *
switchBase :: Int -> Int -> [Int] -> [Int]
switchBase from to num = toBase to (fromBase from num)

-- convert from base * to base 10
fromBase :: Int -> [Int] -> Int
fromBase b n = round $ foldr1 (+) [fromIntegral d * fromIntegral b **p | (d,p) <- zip (reverse n) [0..]]

-- convert to base * from base 10
toBase :: Int -> Int -> [Int]
toBase _ 0 = [0]
toBase 1 n = error "infinite base one"
toBase b n = (toBase b (div n b)) ++ [(mod n b)]
                        
-- convert decibinary number to decimal value
fromDecibinary :: Int -> Int
fromDecibinary = joinDigits . switchBase 2 10 . reverse . digits
    where digits 0 = [0]
          digits n = (mod n 10) : digits (div n 10)

-- convert list of integer digits to single integer
joinDigits = readInt . concatMap show

-- convert string to integer
readInt = read::String->Int

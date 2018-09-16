-- https://www.hackerrank.com/challenges/2d-array

import Data.List

main :: IO ()
main = do
  temp <- getLines 6
  let array = concatMap (map (read :: String -> Int) . words) temp
  print . maximum $ solve array

getLines :: Int -> IO [String]
getLines 0 = return []
getLines n = do          
        x <- getLine         
        xs <- getLines (n-1)    
        return (x:xs)

solve :: [Int] -> [Int]
solve = map (sum . map snd) . groupBy match' . sort . concatMap build' . zip [0..]
  where build' (i, n) = filter valid' [(i - d, n) | d <- [0,1,2,7,12,13,14]]
        valid' (i, _) = i >= 0 && i <= 21 && mod i 6 < 4
        match' a b    = fst a == fst b

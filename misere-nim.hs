-- https://www.hackerrank.com/challenges/misere-nim-1

import Control.Monad
import Data.Bits
import Data.List

main :: IO ()
main = do
  t <- getLine
  let n = read t :: Int
  forM_ [1..n] $ \i -> do
    num <- getLine
    piles <- getLine
    putStrLn $ if solve . map read $ words piles then "First" else "Second"

solve :: [Int] -> Bool
solve ns = (/=0) $ foldr xor s ns
  where s = if all (==1) ns then 1 else 0 :: Int

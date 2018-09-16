-- https://www.hackerrank.com/contests/world-codesprint-8/challenges/sherlock-and-nim

import Control.Monad
import Data.Bits
import Data.List

-- sherlock nim
-- number of piles is a prime number as stated by the challenge
-- the only even prime is 2 which reduces to a Wythoff game
-- otherwise nimber always equals xor(piles[])
nimber :: [Int] -> Int
nimber [a,b] = if sort [a,b] `elem` take (1 + max a b) wythoffPairs then 0 else 1
nimber xs = if mod (length xs) 2 == 1 then foldr xor 0 xs else error "You lied! That's not a prime!"

-- Wythoff array pairs
-- https://en.wikipedia.org/wiki/Wythoff_array
wythoffPairs :: [[Int]]
wythoffPairs = [map floor [x * phi, x * phi**2] | x <- [0..]]
    where phi =  (1 + sqrt 5) / 2

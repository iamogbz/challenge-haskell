-- https://www.hackerrank.com/challenges/decibinary-numbers

import Data.List
import Data.Bits

main :: IO ()
main = interact $ unlines . map (show . decibinary . readInt) . tail . lines

-- get decibinary value at a given index
decibinary :: Int -> Int
decibinary = (concatMap genDecibinary [0..] !!)
-- TODO!! UNDERSTAND HOW THE HELL THIS WORKS!!!
-- generate all valid decibinary values of decimal number
genDecibinary :: Int -> [Int]
genDecibinary = gen 20 0
    -- d is num of digits, v is current integer built, s is decimal value
    where gen d v s
            | s < 0 || s > 9 * (shiftL 1 (d+1) - 1) = []
            | s == 0 && d == -1 = [v]
            | otherwise = concat [gen (d-1) (v*10 + i) (s - i * shift 1 d) | i <- [0..9]]

-- convert to base * from base *
switchBase :: Int -> Int -> [Int] -> [Int]
switchBase to from = toBase to . fromBase from

-- convert from base * to base 10
fromBase :: Int -> [Int] -> Int
fromBase b n = round $ sum [fromIntegral d * fromIntegral b **p | (d,p) <- zip (reverse n) [0..]]

-- convert to base * from base 10
toBase :: Int -> Int -> [Int]
toBase _ 0 = [0]
toBase 1 n = error "infinite base one"
toBase b n = toBase b (div n b) ++ [mod n b]

-- convert decibinary number to decimal value
fromDecibinary :: Int -> Int
fromDecibinary = fromDigits . switchBase 10 2 . reverse . toDigits

-- convert single integer to list of integer toDigits
toDigits 0 = []
toDigits n = mod n 10 : toDigits (div n 10)

-- convert list of integer digits to single integer
fromDigits = readInt . concatMap show

-- convert string to integer
readInt = read::String->Int

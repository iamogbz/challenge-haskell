import System.IO
import Data.List

-- main :: IO ()
main = interact $ unlines . map show . runningMedian . map read . drop 1 . lines

-- cascading median
-- runningMedian :: [Int] -> [Double]
runningMedian = rmed 0 []
    where -- rmed :: Int -> [Int] -> [Int] -> [Double]
          rmed _ _ [] = []
          rmed l s (x:xs) = let sx = insert x s
                                ln = l + 1
                            in (median ln sx):rmed ln sx xs

-- median :: Size, List -> Double
median l xs = toDP 1 $ if even l 
                then (/2) . realToFrac . sum . take 2 $ drop (n-1) xs 
                else realToFrac $ xs !! n
    where n = div l 2 -- middle of list
          -- round to specific number of decimal places
          toDP n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

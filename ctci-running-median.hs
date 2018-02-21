import System.IO
import Data.List

-- main :: IO ()
main = interact $ unlines . map show . runningMedian . map read . drop 1 . lines

-- runningMedian :: [Int] -> [Double]
runningMedian = rmed emptyBList
    where -- rmed :: BList -> [Int] -> [Double]
          rmed _ [] = []
          rmed b (x:xs) = let bs = binsert x b in (median bs):rmed bs xs

-- median :: BList -> Double
median (left,r:ight,s1,s2) = toDP 1 $ if s1 < s2 then r else (r + head left)/2
    where
          -- round to specific number of decimal places
          toDP n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

-- custom data structure
type BList = ([Int],[Int],Int,Int)
emptyBList = ([],[],0,0)
binsert x ([],[],_,_) = ([],[x],0,1)
binsert x ([],r:ight,_,s2) = if r > x then ([x],r:ight,1,s2)
                             else ([r], insert x ight,1,s2)
binsert x (l:eft,r:ight,s1,s2) | s1 < s2 = if l < x then (r:l:eft,insert x ight,s1+1,s2)
                                           else (l:insert' x eft,r:ight,s1+1,s2)
                               | otherwise = if r > x then (insert' x eft,l:r:ight,s1,s2+1)
                                             else (l:eft,r:insert x ight,s1,s2+1)
                               where insert' x = insertBy (\a b -> compare b a) x

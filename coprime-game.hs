import Data.Map (Map)
import qualified Data.Map as Map

(!) = (Map.!)

main :: IO ()
main = do
    temp <- getLine
    let n = read temp :: Int
    temp <- getLine
    let n1 = map (read::String->Int) $ words temp
    temp <- getLine
    let n2 = map (read::String->Int) $ words temp
    putStrLn . show $ maxBPM coprime n1 n2 Map.empty

maxBPM :: (Int -> Int -> Bool) -> [Int] -> [Int] -> Map Int Int -> Int
maxBPM _ [] _ _               = 0
maxBPM func (x:xs) ys matches = i + maxBPM func xs ys m
                              where i = if hasMatch then 1 else 0
                                    (hasMatch, m, _) = bpm func x ys [] Map.empty matches

bpm :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int] 
    -> Map Int Int -> Map Int Int-> (Bool, Map Int Int, Map Int Int)
bpm _ _ [] _ seen matches         = (False, matches, seen) 
bpm func x (y:ys) yz seen matches = 
    if not $ coprime x y && Map.notMember y seen 
      then if Map.notMember y matches
             then (True, (Map.insert y x matches), (Map.insert y x seen))
             else if hasMatch
                  then (True, (Map.insert y x m), s)
                  else bpm func x ys (y:yz) (Map.insert y x s) m
      else bpm func x ys (y:yz) seen matches
    where (hasMatch, m, s) = bpm func (matches ! y) ((y:ys)++yz) [] (Map.insert y x seen) matches

coprime :: Int -> Int -> Bool
coprime a b = (==1) $ gcd a b

-----------------------------

common xs ys = [ x | x <- xs , y <- ys, x == y]
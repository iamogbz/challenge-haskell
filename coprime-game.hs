import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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
                                    (hasMatch, m, _) = bpm func x ys [] Set.empty matches

bpm :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int] 
    -> Set Int -> Map Int Int-> (Bool, Map Int Int, Set Int)
bpm _ _ [] _ seen matches         = (False, matches, seen) 
bpm func x (y:ys) yz seen matches = 
    case not $ coprime x y && Set.notMember y seen of
      True -> case Map.notMember k matches of
                True -> (True, (Map.insert k x matches), (Set.insert y seen))
                _    -> case bpm func (matches ! k) ((y:ys)++yz) [] (Set.insert y seen) matches of
                          (True, m, s) -> (True, (Map.insert k x m), s)
                          (_, m, s)    -> bpm func x ys (y:yz) (Set.insert y s) m
      _    -> bpm func x ys (y:yz) seen matches
    where k = length yz

coprime :: Int -> Int -> Bool
coprime a b = (==1) $ gcd a b

-----------------------------

common xs ys = [ x | x <- xs , y <- ys, x == y]
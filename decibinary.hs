import Numeric
import Data.Char
import Data.List
import qualified Data.Set as Set

main :: IO ()
main = interact $ unlines . map show . tail . lines
        
deciBin :: Int -> Int
deciBin = (deciBins [0] !!)
  where deciBins [] = []
        deciBins xs = xs++(deciBins . filter (\x -> x `mod` 10 /= 9) . unique . concat $ [[x+1, x+9, toBin $ 1 + fromBin x] | x <- xs])

unique :: Ord a => [a] -> [a]
unique = map head . group . sort

toBin :: Int -> Int
toBin 0 = 0
toBin n = read . concat . map show $ reverse (helper n)
  where helper 0 = []
        helper n = let (q,r) = n `divMod` 2 in r : helper q

fromBin :: Int -> Int
fromBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0 . show
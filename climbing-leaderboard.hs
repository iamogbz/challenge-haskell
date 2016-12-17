import System.IO
import Data.List

main :: IO ()
main = interact $ unlines . solve . lines

solve :: [String] -> [String]
solve (_:s:_:l:_) = reverse . map show $ ranking ranks levels
  where (scores:levels:_) = map (map (read::String -> Int) . words) [s,l]
        ranking = ranking' []
        ranking' rs _ [] = rs
        ranking' rs [] lz = [1 | l<-lz]++rs
        ranking' rs ((r,s):rnks) (l:lz) = if s == l
                                            then ranking' (r:rs) rnks lz
                                            else if s < l
                                              then ranking' rs rnks (l:lz)
                                              else ranking' ((r+1):rs) ((r,s):rnks) lz
        ranks = reverse . zip [1..] . map head $ group scores
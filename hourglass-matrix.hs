import Data.List

main :: IO ()
main = do
  temp <- getLines 6
  let array = concat $ map (map (read :: String -> Int) . words) temp
  putStrLn . show . maximum $ extract array
    
getLines :: Int -> IO [String]
getLines 0 = return []
getLines n = do          
        x <- getLine         
        xs <- getLines (n-1)    
        return (x:xs)

extract :: [Int] -> [Int]
extract = map (sum . map snd) . groupBy match' . sort . concat . map build' . zip [0..]
  where build' (i, n) = filter (\x -> fst x >= 0) [(i-d, n) | d <- [0,1,2,7,12,13,14]]
        match' a b = fst a == fst b



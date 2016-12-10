import Data.List

main :: IO ()
main = do
    t <- getLine
    let (n:_) = map (read::String->Int) $ words t
    t <- getLine
    let fs = map (read::String->Int) $ words t
    let res = mul . reverse $ factors n fs
    putStrLn $ if n == (head . reverse) res then concat . map (\x -> (show x)++" ") $ res else "-1"
    
mul :: [Int] -> [Int]
mul fs = mul' 1 fs
  where mul' n [] = [n]
        mul' n (f:fs) = n : (mul' (n*f) fs)

factors :: Int -> [Int] -> [Int]
factors n ns =
  case factors' of
    [] -> []
    _  -> factors' ++ factors (n `div` (head factors')) ns
  where factors' = take 1 $ filter (\x -> (n `mod` x) == 0) $ (reverse . sort) ns
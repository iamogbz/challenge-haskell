import Data.List

-- INCOMPLETE SOLUTION!!!
-- doesn't work for some test cases e.g.
-- 36 [18,9,4] - this should give 1 4 36 [4,9] but it fails
-- 24 [4,6,8] etc.
-- probably do two passes, first from largest to smallest 
-- and then in reverse for elements not used

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
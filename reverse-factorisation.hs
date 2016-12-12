import Data.List
import Data.Maybe

main :: IO ()
main = do
    t <- getLine
    let (n:_) = map (read::String->Int) $ words t
    t <- getLine
    let fs = map (read::String->Int) $ words t
        s = mul . reverse . fromMaybe [] $ solve n fs
    putStrLn $ if s == [0] then "-1" else concat . map (\x -> (show x)++" ") $ s
    
solve :: Int -> [Int] -> Maybe [Int]
solve n []     = Nothing
solve n fs = case n == (last . sort . mul) s of
  True -> Just s
  _    -> solve n ns
  where s = factors n (f:ns) 
        (f:ns) = reverse $ sort fs
    
mul :: [Int] -> [Int]
mul [] = [0]
mul fs = mul' 1 fs
  where mul' n [] = [n]
        mul' n (f:fs) = n : (mul' (n*f) fs)

factors :: Int -> [Int] -> [Int]
factors n ns =
  case factors' of
    [] -> []
    _  -> factors' ++ factors (n `div` (head factors')) ns
  where factors' = take 1 $ filter (\x -> (n `mod` x) == 0) ns
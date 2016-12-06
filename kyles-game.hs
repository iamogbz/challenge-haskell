import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad

main :: IO ()
main = do
  t <- getLine
  let n = read t :: Int
  forM_ [1..n] $ \i -> do
    num <- getLine
    pins <- getLine
    putStrLn $ if solve 0 pins then "WIN" else "LOSE"

pinsToInt :: String -> Int
pinsToInt ps = binToInt [if p == 'I' then '1' else '0' | p <- ps]
  where binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

solve :: Int -> String -> Bool
solve n s = case moves s of 
              []     -> odd n
              (x:xs) -> foldr control (solve (n + 1) x) [solve (n + 1) m | m <- xs]
            where control = if odd n then (&&) else (||)

-- get all moves possible from a state
moves :: String -> [String]
moves m = mov (reverse m) [] []
  where mov [] _ ms = ms
        mov ('I':[]) ys ms = ('X':ys):ms
        mov ('I':'I':xs) ys ms = mov ('I':xs) ('I':ys) $ map (reverse xs ++) [('X':'X':ys),('I':'X':ys)] ++ ms
        mov ('I':x:xs) ys ms = mov xs (x:'I':ys) $ (reverse xs ++ (x:'X':ys)) : ms
        mov (x:xs) ys ms = mov xs (x:ys) ms


import Control.Monad
import Data.Bits
import Data.List

main :: IO ()
main = do
  t <- getLine
  let n = read t :: Int
  forM_ [1..n] $ \i -> do
    num <- getLine
    pins <- getLine
    putStrLn $ if solve pins then "WIN" else "LOSE"

solve :: String -> Bool
solve = (/=0) . foldr xor 0 . map (nimber . length) . filter ((=='I') . head) . group

-- grundy numbers
nimber :: Int -> Int
nimber = (map g [0..] !!)
  where
    g 0 = 0
    g n = mex $
        [ nimber i `xor` nimber (n-1-i) | i <- [0..n-1] ] ++
        [ nimber i `xor` nimber (n-2-i) | i <- [0..n-2] ]

-- minimum excluded numbers
mex :: [Int] -> Int
mex xs = (\\) [0..] xs !!0
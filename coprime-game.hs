import Data.List (group, sort, sortBy)

main :: IO ()
main = do
    temp <- getLine
    let n = read temp :: Int
    temp <- getLine
    let n1 = map (read::String->Int) $ words temp
    temp <- getLine
    let n2 = map (read::String->Int) $ words temp
    let graph = makeBPGraph n1 n2;
    putStrLn . show $ calcBPMax graph

calcBPMax :: [[Bool]] -> Int
calcBPMax _ = 0

makeBPGraph :: [Int] -> [Int] -> [[Bool]]
makeBPGraph xs ys = [mapBP x ys | x <- xs]
                  where mapBP f = map $ not . coprime f

coprime :: Int -> Int -> Bool
coprime a b = (==1) $ gcd a b

-----------------------------

common xs ys = [ x | x <- xs , y <- ys, x == y]
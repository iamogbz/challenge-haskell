import Data.List (group, sort, sortBy)

main :: IO ()
main = do
    temp <- getLine
    let n = read temp :: Int
    temp <- getLine
    let n1 = map (read::String->Int) $ words temp
    temp <- getLine
    let n2 = map (read::String->Int) $ words temp
    putStrLn . show $ solver (primeFactorSort n1) (primeFactorSort n2) [] 0
    
type FactoredInt = (Int, [Int]);

coprime :: Int -> Int -> Bool
coprime a b = (==1) $ gcd a b

solver :: [FactoredInt] -> [FactoredInt] -> [FactoredInt] -> Int -> Int
solver [] _ _ score             = score
solver _ [] [] score            = score
solver (_:xs) [] seen score     = solver xs seen [] score
solver ((d, dfs):xs) ((e, efs):ys) seen score = if null $ common dfs efs -- empty means coprime
                                                then solver ((d, dfs):xs) ys ((e, efs):seen) score
                                                else solver xs (ys++seen) [] (score + 1)


primeFactorSort :: [Int] -> [FactoredInt]
primeFactorSort ns = sortBy f_compare factored
              where factored = [(n, primeFactors n) | n <- ns]
                    f_compare (n1, npf1) (n2, npf2) = if (length npf1) == (length npf2)
                                                         then compare n1 n2 
                                                         else compare (length npf1) (length npf2)
                                    
factorize :: Int -> Int -> [Int]
factorize _ 1 = [] 
factorize d n 
    | d * d > n = [n]
    | n `mod` d == 0 = d : factorize d (n `div` d)
    | otherwise = factorize (d + 1) n

primeFactors :: Int -> [Int]
primeFactors = map head . group . sort . factorize 2

common xs ys = [ x | x <- xs , y <- ys, x == y]
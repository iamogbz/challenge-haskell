import Data.List

nimber :: Int -> Int
nimber = (map g [0..] !!)
    where
        g 0 = 1
        g x = mex $ map (nimber . fromBase 26) (possible $ toBase 26 x)
    
-- possible moves from a state
possible :: (Num a) => [Int] -> [[a]]
possible xs = if sorted xs
                then [map fromIntegral ((++) (map (min i) (take c xs)) (drop c xs)) | (n,c) <- zip xs [1..], i <- [0..n-1]]
                else []
    
-- check if list is sorted
sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted (x:xs) = foldr (\a f b -> (a >= b) && f a) (const True) xs x

-- minimum excluded numbers
-- throws error on empty list
mex :: [Int] -> Int
mex = head . (\\) [0..]

-- convert from base * to base 10
fromBase :: (Enum a, Floating a, RealFrac a) => a -> [a] -> Int
fromBase b xs = foldr (+) 0 $ zipWith (\x y -> fromIntegral $ round (x * (b**y))) xs [0..]

-- convert to base * from base 10
toBase :: Int -> Int -> [Int]
toBase _ 0 = []
toBase b n = (mod n b):toBase b (div n b)
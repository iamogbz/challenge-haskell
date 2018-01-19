import Control.Monad
import Data.Bits
import Data.List

-- sherlock nim
snim :: [Int] -> Int
snim xs = mex (map snim (snims xs))

-- minimum excluded numbers
mex :: [Int] -> Int
mex = head . (\\) [0..]

-- next nim states
nims :: [Int] -> [[Int]]
nims xs = snims xs ++ rnims xs []

-- regular nim states
rnims :: [Int] -> [Int] -> [[Int]]
rnims [] _ = []
rnims (x:xs) s = [i:(xs++s) | i <- [0..x-1]] ++ rnims xs (x:s)

-- next sherlock nim states
snims :: [Int] -> [[Int]]
snims xs = if n > 0 then [map (subtract i) xs | i <- [1..n]] else []
           where n = minimum xs

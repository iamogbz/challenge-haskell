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
nims xs = snims xs ++ rnims xs

-- regular nim states
rnims :: [Int] -> [[Int]]
rnims xs = r xs []
           where r [] _ = []
                 r (y:ys) s = [i:(ys++s) | i <- [0..y-1]] ++ r ys (y:s)

-- next sherlock nim states
snims :: [Int] -> [[Int]]
snims xs = if n > 0 then [map (subtract i) xs | i <- [1..n]] else []
           where n = minimum xs

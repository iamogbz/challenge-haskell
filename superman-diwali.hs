import Data.Maybe (fromMaybe)
import Data.List (group, sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    t <- getLine
    let (n:h:i:_) = map (read::String->Int) $ words t
    ls <- getLines n
    let scene = levels $ concat [building i (map (read::String->Int) $ words b) | (b, i) <- zip ls [1..]]
    putStrLn $ show scene
    let roofs = routes i [] (0,[]) scene
    putStrLn $ show roofs
    
getLines :: Int -> IO [String]
getLines 0 = return []
getLines n = do          
        x <- getLine         
        xs <- getLines (n-1)    
        return (x:xs)

-- Floor(building id, num of people)
type Floor = (Int, Int)
ground = (0, 0)
type Building = [(Int, Floor)]
type Level = (Int, [Floor])
type SortedLevel = (Int, (Floor, Floor))

building :: Int -> [Int] -> Building
building _ (0:_) = []
building i (_:n) = [(head x, (i, (length x))) | x <- (group . sort) n]

levels :: Building -> [(Int, [Floor])]
levels bs = Map.toList $ Map.fromListWith (++) [(k, [v]) | (k, v) <- bs]

routes :: Int -> [SortedLevel] -> Level -> [Level] -> SortedLevel
routes jump s l []           = mysort l
routes jump s l ((i, fs):xs) = routes jump (sorted:s) updated xs
                             where sorted = mysort l
                                   updated = update (sLookup (i+jump) s) l (i, fs)

mysort :: Level -> SortedLevel
mysort (f, []) = (f, (ground, ground))
mysort (f, (x:[])) = (f, (x, ground))
mysort (f, xs) = (f, (best, nextBest)) 
               where (best:nextBest:_) = sortBy fCompare xs
                     fCompare (_, n1) (_, n2) = compare n1 n2

update :: SortedLevel -> Level -> Level -> Level
update jumplevel lastlevel (i, fs) = (i, map (fUpdate i jumplevel lastlevel) fs)

fUpdate :: Int -> SortedLevel -> Level -> Floor -> Floor
fUpdate i (_, ((a, best), (_, nextBest))) (_, ls) (b, score) = (b, score + (max lscore jscore))
                                                             where lscore = fsLookup i ls
                                                                   jscore = if i == a then nextBest else best

sLookup :: Int -> [SortedLevel] -> SortedLevel
sLookup _ []            =  (0, (ground, ground))
sLookup key ((x,y):xys)
    | key == x          =  (x,y)
    | otherwise         =  sLookup key xys
                                         
fsLookup :: Int -> [Floor] -> Int
fsLookup _ []            =  0
fsLookup key ((x,y):xys)
    | key == x          =  y
    | otherwise         =  fsLookup key xys
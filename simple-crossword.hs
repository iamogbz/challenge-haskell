import Data.List (transpose, permutations, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

type Grid = [String]
type Words = [String]

main :: IO ()
main = do
  grid <- getLines 10
  t_words <- getLine
  let ws = splitBy ';' t_words
  putStrLn . intercalate "\n" . head $ allPossible grid ws

getLines :: Int -> IO [String]
getLines 0 = return []
getLines n = do          
        x <- getLine         
        xs <- getLines (n-1)    
        return (x:xs)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = filter (not . null) . foldr f [[]] 
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

allPossible :: Grid -> Words -> [Grid]
allPossible grid words = unique . filter (not . null) $ map (drillDown grid) (permutations words)
  where drillDown grid [] = grid
        drillDown grid (w:ws) = concat $ map (\g -> drillDown g ws) (nextPossible grid w)

nextPossible :: Grid -> String -> [Grid]
nextPossible grid word = unique
  (map (fromMaybe []) . filter (/=Nothing) $ [tryWordOnGrid grid word]) ++
  (map (rotateGridR . fromMaybe []) . filter (/=Nothing) $ [tryWordOnGrid rotatedGrid word])
  where rotatedGrid = rotateGridL grid;

unique :: Ord a => [a] -> [a]
unique = unique' Set.empty where
  unique' _ [] = []
  unique' a (b : c) = if Set.member b a
    then unique' a c
    else b : unique' (Set.insert b a) c

rotateGridR :: Grid -> Grid
rotateGridR = transpose . reverse

rotateGridL :: Grid -> Grid
rotateGridL = reverse . transpose

tryWordOnGrid :: Grid -> String -> Maybe Grid
tryWordOnGrid [] _          = Nothing
tryWordOnGrid (row:rs) word = 
  if matchAttempt == word then Just ((p++matchAttempt++s):rs)
  else case tryWordOnGrid rs word of 
    Just grid -> Just (row:grid)
    _         -> Nothing
  where (p:matchAttempt:s:_) = matchWordToRow word row
    
matchWordToRow :: String -> String -> [String]
matchWordToRow word row = 
  case extractFreeSpace row "" "" of
    (p:m:s:_) -> [p,replaceIfMatch m word,s]

extractFreeSpace :: String -> String -> String -> [String]
extractFreeSpace [] p m     = [p, m, ""]
extractFreeSpace (c:cs) p m = 
  if isBlock c then 
    if length m > 1 then [p, m, c:cs]
    else extractFreeSpace cs (p++m++[c]) ""
  else extractFreeSpace cs p (m++[c])
  
replaceIfMatch :: String -> String -> String
replaceIfMatch toMatch word = 
  if length toMatch == length word then
    if replaced == word then word
    else toMatch
  else toMatch
  where replaced = map (\(a,b) -> if isFree a then b else if a == b then b else '?') $ zip toMatch word

isFree = (=='-')
isBlock = (=='+')

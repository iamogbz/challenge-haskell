import Data.List (transpose, permutations, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

type Board = [String]
type Words = [String]

main :: IO ()
main = do
  bd <- getLines 10
  t_ws <- getLine
  let ws = splitBy ';' t_ws
  putStrLn . intercalate "\n" . head $ allPossible bd ws

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

allPossible :: Board -> Words -> [Board]
allPossible bd ws = unique . filter (not . null) $ map (allPossible' bd) (permutations ws)
  where allPossible' bd [] = bd
        allPossible' bd (w:ws) = concat $ map (\b -> allPossible' b ws) (nextPossible bd w)

nextPossible :: Board -> String -> [Board]
nextPossible bd w = unique
  (map (fromMaybe []) $ filter (/=Nothing) [tryWordOnBoard bd w]) ++
  (map (rotateBoard . fromMaybe []) $ filter (/=Nothing) [tryWordOnBoard rotatedBoard w])
  where rotatedBoard = rotateBoard' bd;

unique :: Ord a => [a] -> [a]
unique = unique' Set.empty where
  unique' _ [] = []
  unique' a (b : c) = if Set.member b a
    then unique' a c
    else b : unique' (Set.insert b a) c

rotateBoard :: Board -> Board
rotateBoard = transpose . reverse

rotateBoard' :: Board -> Board
rotateBoard' = reverse . transpose

tryWordOnBoard :: Board -> String -> Maybe Board
tryWordOnBoard [] _          = Nothing
tryWordOnBoard (row:rs) w = 
  if pw == w then Just ((p++pw++s):rs)
  else case tryWordOnBoard rs w of 
    Just bd -> Just (row:bd)
    _         -> Nothing
  where (p,pw,s) = tryWordOnLine w row
    
tryWordOnLine :: String -> String -> (String, String, String)
tryWordOnLine w row = 
  case splitLine row of
    (p,m,s) -> (p,tryWordInSpace m w,s)

splitLine :: String -> (String, String, String)
splitLine = splitLine' "" ""
  where splitLine' p m []     = (p,m,"")
        splitLine' p m (c:cs) = 
          if c=='+' then 
            if length m > 1 then (p, m, c:cs)
            else splitLine' (p++m++[c]) "" cs
          else splitLine' p (m++[c]) cs
  
tryWordInSpace :: String -> String -> String
tryWordInSpace s w = 
  if length s == length w then if p == w then w else s else s
  where p = map (\(a,b) -> if a=='-' || a==b then b else '?') $ zip s w

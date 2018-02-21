import System.IO

main :: IO ()
main = interact $ unlines . map isBalanced . drop 1 . lines

isBalanced x = if matchBrackets [] x then "YES" else "NO"

matchBrackets [] [] = True
matchBrackets (b:bs) [] = False
matchBrackets [] (x:xs) = matchBrackets [x] xs
matchBrackets (b:bs) (x:xs) = if match b x then matchBrackets bs xs
                              else matchBrackets (x:b:bs) xs
    where match '[' ']' = True
          match '(' ')' = True
          match '{' '}' = True
          match _ _     = False
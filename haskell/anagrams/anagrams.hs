import Control.Monad
import Data.List
-- https://www.hackerearth.com/practice/basic-programming/input-output/basics-of-input-output/practice-problems/algorithm/anagrams-651/
main = do
  t <- getLine
  forM [1..(read t :: Int)] (\_->do
    n1 <- getLine
    n2 <- getLine
    putStrLn $ show $ ((length n1) + (length n2)) - (2 * (length $ anagrams (sort n1) (sort n2))))

anagrams :: String -> String -> String
anagrams [] [] = []
anagrams (x:_) [] = []
anagrams [] (x:_) = []
anagrams (x:xs) n2 =
  if isInfixOf [x] n2 then x : (anagrams xs (tail (dropWhile (/= x) n2))) -- list contains search term, remove from n2
  else anagrams xs n2 -- n2 doesn't contain x, so drop it

import Data.List
import Data.Char
import Data.Maybe
-- https://www.hackerearth.com/practice/basic-programming/input-output/basics-of-input-output/practice-problems/algorithm/cipher-1/submissions/
main :: IO ()
main = do
  text <- getLine
  n <- getLine
  putStrLn $ map (\c -> shift (read n::Int) c) text

shift :: Int -> Char -> Char
shift n c
  | isCapitalized = shiftN n c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  | isLetter = shiftN n c "abcdefghijklmnopqrstuvwxyz"
  | otherwise = if isInfixOf [c] "0123456789" then shiftN n c "0123456789" else c
  where isLetter = isInfixOf [c] "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        isCapitalized = isLetter && isInfixOf [c] "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

shiftN :: Int -> Char -> String -> Char
shiftN n c s = do
  let index = fromJust $ elemIndex (c) s
  s!!(mod (index + n) (length s))
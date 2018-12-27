import Control.Monad
import Data.Function (fix)

main :: IO [()]
main = do
  t <- getLine
  forM [1..(read t :: Int)] (\_->do
    input <- getLine
    putStrLn $ show $ pentagonalMemo $ (read input :: Int))

pentagonal :: Int -> Int
pentagonal n
  | n == 1 = 1
  | n == 2 = 5
  | otherwise = do
    let x = n - 1
    (x * 5) + (accumulator x 2 2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

pentagonalMemo :: Int -> Int
pentagonalMemo = memoize pentagonal
    
accumulator :: Int -> Int -> Int -> Int
accumulator i p o
  | i <= 2 = o 
  | otherwise = accumulator (i - 1) (p + 3) (o + p + 3)


import Control.Monad
import Data.Function (fix)

main = do
  c <- getLine
  forM [1..(read c :: Int)] (\_->do
    n <- getLine
    putStrLn $ show $ (fibMemo (read n :: Int)) `mod` (10^8+7))

fib :: (Int -> Integer) -> Int -> Integer
fib f 0 = 0
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)
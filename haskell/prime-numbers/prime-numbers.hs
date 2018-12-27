main :: IO ()
main = do
  n <- getLine
  putStrLn $ primes (read n :: Int) []

primes :: Int -> [Int]
primes n r = if isPrime n then primes (n-1) (n : r) else primes (n-1) r
primes 0 r = r
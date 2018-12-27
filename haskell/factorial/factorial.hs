main :: IO ()
main = do
  input <- getLine
  print $ factorial (read input ::Int)

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial n = factorial (n - 1) * n
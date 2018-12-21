import Control.Monad

is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n 
  | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
  | otherwise = True

is_prime_rec_left :: String -> Bool
is_prime_rec_left s 
  | 1 == length s = is_prime (read s :: Int)
  | otherwise = (is_prime (read s :: Int)) && is_prime_rec_left (tail s)

is_prime_rec_right :: String -> Bool
is_prime_rec_right s 
  | 1 == length s = is_prime (read s :: Int)
  | otherwise = (is_prime (read s :: Int)) && is_prime_rec_right (take ((length s) - 1) s)

determineFate :: String -> String
determineFate x 
  | noZeros && primeLeftRec && primeRightRec = "CENTRAL"
  | noZeros && primeLeftRec && not primeRightRec = "LEFT"
  | noZeros && not primeLeftRec && primeRightRec = "RIGHT"
  | otherwise = "DEAD"
  where noZeros = null $ filter (\c -> c == '0') x
        primeLeftRec = is_prime_rec_left x
        primeRightRec = is_prime_rec_right x

main = do
  input <- getLine
  inputs <- replicateM (read input) getLine
  mapM_ putStrLn $ map determineFate inputs
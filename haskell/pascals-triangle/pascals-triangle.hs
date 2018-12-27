import Control.Monad

main :: IO ()
main = do
  x <- getLine
  mapM_ putStrLn $ solve x

solve :: String -> [String]
solve n = reverse $ map (\x -> unwords $ map (\y -> show y) x) $ _solve (read n :: Int) []

_solve :: Int -> [[Int]] -> [[Int]]
_solve n o
  | l <= 2 = _solve n $ (replicate l 1) : o
  | n < l = o
  | otherwise = _solve n $ (rowFrom $ head o) : o
  where l = 1 + length o

rowFrom :: [Int] -> [Int]
rowFrom x = 1 : sumPairs x ++ [1]

sumPairs :: [Int] -> [Int]
sumPairs [] = []
sumPairs (x:[]) = []
sumPairs (x: xs) = (x + (head xs)) : (sumPairs xs)


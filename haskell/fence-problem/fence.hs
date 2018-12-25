-- -- https://www.hackerrank.com/challenges/john-and-fences/problem


main :: IO ()
main = do
  rm <- getLine
  inputs <- getLine
  -- print $ iMap (\x y -> x + y) $ map (\x -> read x :: Int) $ words $ inputs
  print $ _solve 0 [0] $ map (\x -> read x :: Int) $ words $ inputs

-- solve :: [Int] -> Int
-- solve input = _solve 0 [0] input
--   -- foldl (\acc x -> acc + x) 0 [1,2,3,4]

incAll :: [Int] -> [Int]
incAll = map (\x -> x + 1)

_solve :: Int -> [Int] -> [Int] -> Int
_solve maxArea seqs [] = maxArea
_solve maxArea seqs (x:xs)
  | x > l = _solve maxArea ((incAll seqs) ++ replicate (x - l) 1) xs -- incease and add (x - l) to seqs
  | x == l = _solve maxArea (incAll seqs) xs -- if no height change, increment all sequences
  | x < l = do
    let newSeqs = incAll $ take x seqs
    let endSeqs = drop x seqs
    let maxEnded = maximum $ maxArea : _iMap (\n idx -> n * (idx + 1 + x)) 0 endSeqs
    _solve maxEnded newSeqs xs

  where l = length seqs

-- iMap :: (Int -> Int -> Int) -> [Int] -> [Int]
-- iMap fn input = _iMap fn 0 input

_iMap :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
_iMap fn idx [] = []
_iMap fn idx (x:[]) = [fn x idx]
_iMap fn idx (x:xs) = fn x idx : _iMap fn (idx + 1) xs

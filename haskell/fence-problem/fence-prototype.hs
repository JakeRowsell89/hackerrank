-- -- https://www.hackerrank.com/challenges/john-and-fences/problem

main :: IO ()
main = do
  rm <- getLine
  inputs <- getLine
  print $ getMaxSurface 1 0 $ map (\x -> read x :: Int) $ words $ inputs

getMaxSurface :: Int -> Int -> [Int] -> Int
getMaxSurface h m [] = m
getMaxSurface h m seq = do
  -- get all sequences in seq of height >= h
  let seqs = groupByPredicate (\x -> x >= h) seq
  -- if no sequences found return m
  if null seqs then m
  else do
    -- find longest sequence
    -- m = Max(m, (longest sequence length * h))
    let newM = maximum [m, h * (maximum $ map (length) seqs)]
    -- for each sequence
    maximum $ map (\newSeq -> getMaxSurface (h + 1) newM newSeq) seqs
      

groupByPredicate :: (Int -> Bool) -> [Int] -> [[Int]]
groupByPredicate pred [] = []
groupByPredicate pred i = do
  let l = takeWhile pred i 
  -- if null l then groupByPredicate pred $ tail i 
  if null l then [tail i]
  else [l] ++ (groupByPredicate pred $ dropWhile pred i)

-- groupByPredicate :: (Int -> Bool) -> [Int] -> [[Int]]
-- groupByPredicate pred [] = []
-- groupByPredicate pred i = do
--   let (t, k) = takeWhileAndKeep pred i []
--   if null t then groupByPredicate pred $ tail i 
--   else [t] ++ (groupByPredicate pred k)

-- takeWhileAndKeep :: (Int -> Bool) -> [Int] -> [Int] -> ([Int], [Int])
-- takeWhileAndKeep pred [] t = (t, [])
-- takeWhileAndKeep pred (x:xs) t = if pred x then takeWhileAndKeep pred xs (t ++ [x]) else (t, x:xs)



combine :: [Char] -> [Char] -> String
combine l1 l2 = foldr (\(a,b) c -> [a,b] ++ c) [] $ zip l1 l2

main :: IO ()
main = do
  l1 <- getLine
  l2 <- getLine
  
  putStrLn $ combine l1 l2

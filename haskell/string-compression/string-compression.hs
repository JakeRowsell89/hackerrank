groupSeq :: String -> [String]
groupSeq "" = [""]
groupSeq str = do
  [(takeWhile (\x -> (head str) == x) str)] ++ groupSeq (dropWhile (\x -> (head str) == x) str)

compressSeq :: String -> String
compressSeq x = do
  if length x > 1 then [(head x)] ++ (show $ length x)
  else x

compress :: String -> String
compress str = concat $ map compressSeq $ groupSeq str

main :: IO ()
main = do
  input <- getLine
  putStrLn $ compress input

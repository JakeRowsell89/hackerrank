import qualified Data.Set as Set

main :: IO ()
main = do
  input <- getLine
  putStrLn $ dedupe input Set.empty

dedupe :: String -> Set.Set Char -> String
dedupe str set
  | null str = ""
  | otherwise = 
    if Set.member x set then dedupe xs set
    else x : dedupe xs (Set.insert x set)
  where (x:xs) = str
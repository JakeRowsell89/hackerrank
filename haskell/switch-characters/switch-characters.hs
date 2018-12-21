import Data.List.Split

switchCharacters :: String -> String
switchCharacters s = concat $ map reverse $ chunksOf 2 s

main :: IO ()
main = do
  inputs <- getContents
  mapM_ putStrLn $ map switchCharacters $ tail $ lines inputs

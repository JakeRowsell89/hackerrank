import Control.Monad

main :: IO [()]
main = do
  in1 <- getLine
  in2 <- getLine
  mapM putStrLn $ sharedPrefix in1 in2 ""

sharedPrefix :: String -> String -> String -> [String]

sharedPrefix in1 in2 shared
  | neitherListEmpty && (head in1) == (head in2) = sharedPrefix (tail in1) (tail in2) ((head in1):shared)
  | otherwise = [(strWithLength $ reverse shared), (strWithLength in1), (strWithLength in2)]
  where neitherListEmpty = not $ (null in1 || null in2)

lengthS :: String -> String
lengthS = show . length

strWithLength :: String -> String
strWithLength [] = "0"
strWithLength s = lengthS s ++ (' ' : s)
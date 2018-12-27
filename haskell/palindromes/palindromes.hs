import qualified Data.ByteString as B

main :: IO ()
main = do
  input <- B.getLine
  putStrLn $ if palindrome input then "YES" else "NO"

palindrome :: B.ByteString -> Bool
palindrome s
  | odd l = a == (B.reverse $ B.tail z)
  | otherwise = a == (B.reverse z)
  where l = B.length s
        half = (B.length s) `div` 2
        (a,z) = B.splitAt half) s
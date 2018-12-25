Haskell:

Reading input from files

Line-by-line string processing use `interact`:
```
main :: IO ()
main = interact processLine

processLine :: String -> String
```

Processing a input as a buffer with `getContents`:
```
main :: IO ()
main = do
  inputs <- getContents
  mapM_ putStrLn $ map processLine $ lines inputs

processLine :: String -> String
```
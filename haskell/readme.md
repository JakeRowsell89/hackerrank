Haskell:

Reading input from files

Line-by-line string processing use `interact` e.g.:
```
main :: IO ()
main = interact capitalize

capitalize :: String -> String
capitalize = map Char.toUpper 
```
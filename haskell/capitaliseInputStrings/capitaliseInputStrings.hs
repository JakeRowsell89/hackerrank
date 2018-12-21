import qualified Data.Char as Char

main :: IO ()
main = interact capitalize

capitalize :: String -> String
capitalize = map Char.toUpper 
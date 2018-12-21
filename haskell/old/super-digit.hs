import Control.Applicative
import Control.Monad
import System.IO
import Data.Char

sumDigits :: String -> String
sumDigits x = show $ sum $ map(\x -> digitToInt x) x

super_digit :: String -> String
super_digit x
    | length x < 2 = x
    | otherwise = super_digit(sumDigits x) 

main :: IO ()
main = do
    val <- getLine
    let (h:t) = map(\x -> read x :: Integer)(words val)
    let p = show (h * head t)

    print ( read (super_digit p) :: Int)
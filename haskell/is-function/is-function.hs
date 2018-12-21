-- https://www.hackerrank.com/challenges/functions-or-not/problem

-- input format:
-- T, number of test cases followed by information about the test cases
-- For each test case:
-- N, the number of I/O pairs followed by 
-- N pairs in the format "Int Int"

import Control.Monad
import Data.Char (isSpace)

-- Thanks @eric-normand on Stackoverflow :-)
trim :: String -> String 
trim = f . f
  where f = reverse . dropWhile isSpace

-- Input format is [2, A, A, 1, B, 3, C, C, C] 
-- numbers indicate how many lines following are part of I/O test case
_groupTestCases :: [String] -> [[String]] -> [[String]]
_groupTestCases [] [] = [[]]
_groupTestCases [] result = result
_groupTestCases (x:xs) result = do 
  let (inputs, remaining) = splitAt (read $ x :: Int) xs
  _groupTestCases remaining $ result ++ [inputs]

groupTestCases :: [String] -> [[String]]
groupTestCases l = _groupTestCases l []

-- matchAtEnd :: [a] => a -> [String] -> [String] -> Bool
matchAtEnd endFn in1 in2 = endFn in1 == endFn in2

ioMismatches :: String -> String -> Bool
ioMismatches in1 in2 = do
  matchAtEnd head in1 in2 && (not $ matchAtEnd last in1 in2)

isFunction :: [String] -> String
isFunction (_:[]) = "YES"
isFunction (x:xs) = do
  -- For each I/O, if the same I is listed with a different O it's not a function
  if any (ioMismatches x) xs then "NO" else isFunction(xs)

main :: IO ()
main = do
  inputs <- getContents
  mapM_ putStrLn $ map isFunction $ groupTestCases $ map trim $ tail $ lines inputs

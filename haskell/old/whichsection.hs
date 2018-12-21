{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe


-- Complete the whichSection function below.
whichSection a k i t
    | length a == 0 = i
    | k <= (t + head a) = i
    | otherwise = whichSection (tail a) k (i + 1) (t + head a)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        nkmTemp <- getLine
        let nkm = words nkmTemp

        let n = read (nkm !! 0) :: Int

        let k = read (nkm !! 1) :: Int

        let m = read (nkm !! 2) :: Int

        aTemp <- getLine

        let a = Data.List.map (read :: String -> Int) . words $ aTemp

        let result = whichSection a k 1 0

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

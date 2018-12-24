-- https://www.hackerrank.com/challenges/kmp-fp/submissions/code/93638851

import qualified Data.ByteString as B
import Control.Monad

main = do
    t <- getLine
    forM [1..(read t :: Int)] (\_->do
        n1 <- B.getLine
        n2 <- B.getLine
        putStrLn $ if B.isInfixOf n2 n1 then "YES" else "NO")
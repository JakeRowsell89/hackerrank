-- Enter your code here. Read input from STDIN. Print output to STDOUT

-- multiply all jumps, to get furthest possibility F
-- from smallest S to F, check there are any positions all bunnies can reach

import System.IO
dividesByAll :: [Int] -> Int -> Bool
dividesByAll hops x = null $ filter (\h -> 0 /= mod x h) hops

main :: IO ()
main = do
    num <- getLine
    hopsStr <- getLine
    let hops = map(\x -> read x :: Int)(words hopsStr)
    let f = product hops
    let m = maximum hops
    
    print $ head $ take 1 $ filter (\x -> dividesByAll hops x) [m..f]
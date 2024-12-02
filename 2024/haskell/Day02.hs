import Data.Ix (inRange)
import Data.List (splitAt)
import Common (count)

main = do
    input <- map (map read . words) . lines <$> readFile "inputs/2.txt"
    print $ count isSafe input
    print $ count (any isSafe . removeEachOnce) input

isSafe :: [Int] -> Bool
isSafe xs = all (inRange (-3, -1)) diffs || all (inRange (1, 3)) diffs
    where diffs = zipWith (-) (tail xs) xs

removeEachOnce :: [a] -> [[a]]
removeEachOnce xs = xs : map ((\(a, b) -> init a ++ b) . (`splitAt` xs)) [1..(length xs)]

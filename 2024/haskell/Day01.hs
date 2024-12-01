import Data.List (sort, transpose)
import Common (count)

main = do
    input <- parse <$> readFile "inputs/1.txt"
    print $ sum $ map absDiff $ transpose input
    print $ similarityScore input

absDiff :: [Int] -> Int
absDiff [x, y] = abs (x - y)

similarityScore :: [[Int]] -> Int
similarityScore [l, r] = sum $ map (\n -> n * count (== n) r) l

parse :: String -> [[Int]]
parse = map sort . transpose . map (map read . words) . lines

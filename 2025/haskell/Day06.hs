import Data.List (transpose)

main = do
    input <- parse <$> readFile "inputs/06.txt"
    print $ sum $ map p1 input

p1 :: [String] -> Int
p1 ("*":ns) = product $ map read ns
p1 ("+":ns) = sum $ map read ns

parse :: String -> [[String]]
parse input = map reverse $ transpose $ map words $ lines input

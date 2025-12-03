import Data.Char (digitToInt)

main = do
    input <- map (map digitToInt) . lines <$> readFile "inputs/03.txt"
    print $ sum $ map (joltage 1) input
    print $ sum $ map (joltage 11) input

joltage :: Int -> [Int] -> Int
joltage 0 xs = maximum xs
joltage n xs = largest * (10 ^ n) + joltage (n - 1) (drop 1 (dropWhile (/= largest) xs))
    where largest = maximum $ take (length xs - n) xs

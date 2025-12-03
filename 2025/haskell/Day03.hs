import Data.Char (digitToInt)

main = do
    input <- map (map digitToInt) . lines <$> readFile "inputs/03.txt"
    print $ sum $ map p1 input

p1 :: [Int] -> Int
p1 xs = largest * 10 + maximum (drop 1 (dropWhile (/= largest) xs))
    where largest = maximum $ init xs

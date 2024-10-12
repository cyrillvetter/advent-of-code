import Common (windowsOf)

main = do
    input <- map read . lines <$> readFile "inputs/1.txt"
    print $ countIncreases input
    print $ countIncreases $ map sum $ windowsOf 3 input

countIncreases :: [Int] -> Int
countIncreases input = length $ filter (> 0) $ zipWith (-) (drop 1 input) input

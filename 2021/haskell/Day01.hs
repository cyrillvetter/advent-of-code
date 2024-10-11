{-# LANGUAGE TypeApplications #-}

main = do
    input <- map (read @Int) . lines <$> readFile "inputs/1.txt"
    print $ countIncreases input
    print $ countIncreases $ map sum $ windowsOf 3 input

countIncreases :: [Int] -> Int
countIncreases input = length $ filter (> 0) $ zipWith (-) (drop 1 input) input

windowsOf :: Int -> [a] -> [[a]]
windowsOf size list
    | length list < size = []
    | otherwise = take size list : windowsOf size (drop 1 list)

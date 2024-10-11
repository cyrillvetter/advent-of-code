{-# LANGUAGE TypeApplications #-}

import Data.List.Split (splitOn)

main = do
    input <- parse <$> readFile "inputs/2.txt"
    print $ sum $ map (\row -> maximum row - minimum row) input
    print $ sum $ map findEvenDivision input

parse :: String -> [[Int]]
parse = map (map (read @Int) . splitOn "\t") . lines

findEvenDivision :: [Int] -> Int
findEvenDivision list = head [x `div` y | x <- list, y <- list, x /= y, x `mod` y == 0]

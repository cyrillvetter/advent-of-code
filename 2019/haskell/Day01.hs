main = do
    input <- map read . lines <$> readFile "inputs/1.txt"
    print $ sum $ map (\n -> n `div` 3 - 2) input
    print $ sum $ map calculateFuel input

calculateFuel :: Int -> Int
calculateFuel n
    | calc <= 0 = 0
    | otherwise = calc + calculateFuel calc 
    where calc = n `div` 3 - 2

main = do
    input <- map ((\[x, y] -> (x, read @Int y)) . words) . lines <$> readFile "inputs/2.txt"
    print $ dive input 0 0
    print $ diveWithAim input 0 0 0

dive :: [(String, Int)] -> Int -> Int -> Int
dive [] pos depth = pos * depth
dive ((dir, val):xs) pos depth
    | dir == "forward" = dive xs (pos + val) depth
    | dir == "up" = dive xs pos (depth - val)
    | otherwise = dive xs pos (depth + val)

diveWithAim :: [(String, Int)] -> Int -> Int -> Int -> Int
diveWithAim [] pos depth _ = pos * depth
diveWithAim ((dir, val):xs) pos depth aim
    | dir == "forward" = diveWithAim xs (pos + val) (depth + val * aim) aim
    | dir == "up" = diveWithAim xs pos depth (aim - val)
    | otherwise = diveWithAim xs pos depth (aim + val)

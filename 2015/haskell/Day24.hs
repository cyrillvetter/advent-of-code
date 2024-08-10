import Debug.Trace (trace)

testInput = [1..5] ++ [7..11]

main = do
    input <- map read . lines <$> readFile "inputs/24.txt"
    -- let c = combinations 2 input
    -- print $ length c
    -- -- print c

    -- print $ minimum $ map product $ filter (hasTwoSubGroups (trace "combination" input) 1) c
    print $ hasTwoSubGroups input 1 [11, 109]
    print "Day 24"

hasTwoSubGroups :: [Int] -> Int -> [Int] -> Bool
hasTwoSubGroups all c rem
    | trace "reached" c > length all `div` 2 = False
    | remSum `elem`
        [ s
        | let rest = all `without` rem
        , a <- combinations c rest
        , let s = sum a
        , let b = rest `without` a
        , s == sum b] = True
    | otherwise = hasTwoSubGroups all (c + 1) rem
    where remSum = sum rem

without :: Eq a => [a] -> [a] -> [a]
without a b = filter (`notElem` b) a

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n - 1) xs) ++ combinations n xs

main = do
    input <- readFile "inputs/9.txt"
    print $ score input False 0
    print $ count input False

score :: String -> Bool -> Int -> Int
score [] _ _ = 0
score (x:xs) inGarbage groupCounter
    | x == '!' = score (drop 1 xs) inGarbage groupCounter
    | x == '<' = score xs True groupCounter
    | x == '>' = score xs False groupCounter
    | inGarbage = score xs inGarbage groupCounter
    | x == '{' = score xs inGarbage (groupCounter + 1)
    | x == '}' = groupCounter + score xs inGarbage (groupCounter - 1)
    | otherwise = score xs inGarbage groupCounter

count :: String -> Bool -> Int
count [] _ = 0
count (x:xs) inGarbage
    | x == '!' = count (drop 1 xs) inGarbage
    | not inGarbage && x == '<' = count xs True
    | inGarbage && x == '>' = count xs False
    | inGarbage = 1 + count xs inGarbage
    | otherwise = count xs inGarbage

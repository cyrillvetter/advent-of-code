import Data.List (group, sort)

main = do
    input <- lines <$> readFile "inputs/2.txt"
    print $ part1 input
    print $ findCharDiff input (length (head input) - 1)

part1 :: [String] -> Int
part1 input = countAnyWithLength 2 g * countAnyWithLength 3 g
    where g = map (group . sort) input

countAnyWithLength :: Int -> [[[a]]] -> Int
countAnyWithLength count = length . filter (any (\l -> count == length l))

findCharDiff :: [String] -> Int -> String
findCharDiff (x:xs) searchLen
    | found = map fst (head search)
    | otherwise = findCharDiff xs searchLen
    where search = filter (\e -> length e == searchLen) $ map (filter (uncurry (==)) . zip x) xs
          found = length search == 1
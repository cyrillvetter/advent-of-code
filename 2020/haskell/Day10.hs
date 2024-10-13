import Data.List (sort)

main = do
    input <- sort . map read . lines <$>readFile "inputs/10.txt"
    print input
    print $ part1 input
    print $ part2 input

part1 :: [Int] -> Int
part1 input = count 1 differences * (count 3 differences + 1)
    where differences = zipWith (-) input (0 : input)

part2 :: [Int] -> Int
part2 input = product $ map arrangements grouped
    where grouped = groupAscending input [0]

groupAscending :: [Int] -> [Int] -> [[Int]]
groupAscending [] acc = [acc]
groupAscending (x:xs) (y:ys)
    | x - y == 1 = groupAscending xs (x:y:ys)
    | otherwise = (y:ys) : groupAscending xs [x]

arrangements :: [a] -> Int
arrangements xs = case length xs of
    3 -> 2
    4 -> 4
    5 -> 7
    _ -> 1

count :: Int -> [Int] -> Int
count eq = length . filter (== eq)

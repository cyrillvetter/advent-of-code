import Data.List.Split (splitOn)

main = do
    input <- map parse . lines <$> readFile "inputs/7.txt"
    print $ sum $ map (uncurry part1) input
    print $ sum $ map (uncurry part2) input

part1 :: Int -> [Int] -> Int
part1 exp [x] = if exp == x then x else 0
part1 exp (x:y:xs) = max (part1 exp (x+y:xs)) (part1 exp (x*y:xs))

part2 :: Int -> [Int] -> Int
part2 exp [x] = if exp == x then x else 0
part2 exp (x:y:xs) = maximum
    [ part2 exp (x+y:xs)
    , part2 exp (x*y:xs)
    , part2 exp (concatInts x y:xs) ]

concatInts :: Int -> Int -> Int
concatInts x y = (10 ^ (floor (logBase 10 (fromIntegral y)) + 1)) * x + y

parse :: String -> (Int, [Int])
parse line = (read exp, map read $ words ops)
    where [exp, ops] = splitOn ": " line

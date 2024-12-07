import Data.Word (Word64)
import Data.List.Split (splitOn)

main = do
    input <- map parse . lines <$> readFile "inputs/7.txt"
    print $ sum $ map fst $ filter (uncurry part1) input
    print $ sum $ map fst $ filter (uncurry part2) input

part1 :: Word64 -> [Word64] -> Bool
part1 val [x] = val == x
part1 val (x:y:xs) =
    part1 val (x+y:xs) ||
    part1 val (x*y:xs)

part2 :: Word64 -> [Word64] -> Bool
part2 val [x] = val == x
part2 val (x:y:xs) =
    part2 val (x+y:xs) ||
    part2 val (x*y:xs) ||
    part2 val (concatWords x y:xs)

concatWords :: Word64 -> Word64 -> Word64
concatWords x y = (10 ^ (floor (logBase 10 (fromIntegral y)) + 1)) * x + y

parse :: String -> (Word64, [Word64])
parse line = (read exp, map read $ words ops)
    where [exp, ops] = splitOn ": " line

import Data.Word (Word64)
import Data.List.Split (splitOn)

main = do
    input <- map parse . lines <$> readFile "inputs/7.txt"
    print $ sum $ map (uncurry part1) input
    print $ sum $ map (uncurry part2) input

part1 :: Word64 -> [Word64] -> Word64
part1 exp [x] = if exp == x then x else 0
part1 exp (x:y:xs) = max (part1 exp (x+y:xs)) (part1 exp (x*y:xs))

part2 :: Word64 -> [Word64] -> Word64
part2 exp [x] = if exp == x then x else 0
part2 exp (x:y:xs) = maximum
    [ part2 exp (x+y:xs)
    , part2 exp (x*y:xs)
    , part2 exp (concatWords x y:xs) ]

concatWords :: Word64 -> Word64 -> Word64
concatWords x y = (10 ^ (floor (logBase 10 (fromIntegral y)) + 1)) * x + y

parse :: String -> (Word64, [Word64])
parse line = (read exp, map read $ words ops)
    where [exp, ops] = splitOn ": " line

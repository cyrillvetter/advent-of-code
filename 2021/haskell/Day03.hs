import Data.List (transpose)
import Data.Char (digitToInt)

main = do
    input <- lines <$> readFile "inputs/3.txt"
    print $ part1 input
    print $ part2 input

part1 :: [String] -> Int
part1 input = calc (transpose input) 0 0
    where
        calc :: [String] -> Int -> Int -> Int
        calc [] gamma epsilon = gamma * epsilon
        calc (x:xs) gamma epsilon
            | oneCount > zeroCount = calc xs (gamma + num) epsilon
            | otherwise = calc xs gamma (epsilon + num)
            where zeroCount = length $ filter (== '0') x
                  oneCount = length x - zeroCount
                  num = 2 ^ length xs

part2 :: [String] -> String
part2 input = find (transpose input) input input 0
    where
        find :: [String] -> [String] -> [String] -> Int -> String
        find _ [x] [y] _ = x
        find (t:ts) x y pos =
            let zeroCount = length $ filter (== '0') t
                mostCommon = if zeroCount >= (length t `div` 2) then '0' else '1'
                leastCommon = if mostCommon == '0' then '1' else '0'
                nextX = if length x > 1 then filter (\i -> (i !! pos) == mostCommon) x else x
                nextY = if length y > 1 then filter (\i -> (i !! pos) == leastCommon) y else y
            in find ts nextX nextY (pos + 1)


convert :: String -> Int
convert [] = 0
convert (x:xs) = digitToInt x * (2 ^ length xs) + convert xs
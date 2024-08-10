import Data.Char (digitToInt)
import Data.List (group)

main = do
    input <- map digitToInt <$> readFile "inputs/1.txt"
    let appendedHeadInput = input ++ [head input]

    print $ sum $ map (\g -> head g * (length g - 1)) $ group appendedHeadInput
    print $ sum $ zipWith (halfwayCount input) input [0..]

halfwayCount :: [Int] -> Int -> Int -> Int
halfwayCount digits c i =
    let len = length digits
        halfwayDigit = digits !! ((i + (len `div` 2)) `mod` len)
    in if c == halfwayDigit then c else 0
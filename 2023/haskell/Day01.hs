import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (intercalate)

nums = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

main = do
    input <- lines <$> readFile "inputs/1.txt"
    print $ sum $ map readFirstLast input
    print $ sum $ map (readFirstLast . replaceNums) input

readFirstLast :: String -> Int
readFirstLast = read . (\s -> [head s, last s]) . filter isDigit

replaceNums :: String -> String
replaceNums s = foldr (\(to, with) -> intercalate (to ++ [with] ++ to) . splitOn to) s nums

import Data.List.Split (splitOn)
import Data.List (sort)

main = do
    calories <- sumCalories <$> readFile "inputs/1.txt"
    print $ maximum calories
    print $ sum $ take 3 $ reverse $ sort $ calories

sumCalories :: String -> [Int]
sumCalories = map (sum . map read . lines) . splitOn "\n\n"
import Data.List (sort)
import Data.List.Split (splitOn)

main = do
    input <- map (sort . map read . splitOn "x") . lines <$> readFile "inputs/2.txt"
    print $ sum $ map calculateSize input
    print $ sum $ map calculateRibbon input

calculateSize :: [Int] -> Int
calculateSize [x, y, z] = 2*x*y + 2*y*z + 2*z*x + x*y

calculateRibbon :: [Int] -> Int
calculateRibbon [x, y, z] = 2*x + 2*y + x*y*z

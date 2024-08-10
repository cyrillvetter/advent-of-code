import Data.Set (fromList)
import Data.List (sort)

main = do
    input <- map words . lines <$> readFile "inputs/4.txt"
    print $ length $ filter allUnique input
    print $ length $ filter allUnique $ map (map sort) input

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique list = length list == length (fromList list)
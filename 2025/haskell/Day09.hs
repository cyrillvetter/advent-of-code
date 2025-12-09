import Common (toTuple)
import Data.List.Split (splitOn)

type Tile = (Int, Int)

main = do
    input <- parse <$> readFile "inputs/09.txt"
    print $ maximum $ map area $ cartesianProduct input

cartesianProduct :: Eq a => [a] -> [(a, a)]
cartesianProduct xs = [(a, b) | a <- xs, b <- xs, a /= b]

area :: (Tile, Tile) -> Int
area ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

parse :: String -> [Tile]
parse = map (toTuple . map read . splitOn ",") . lines

import Data.List.Split (splitOn)
import Data.List (intersect)

type Point = (Int, Int)

-- TODO: Improve performance
main = do
    input <- map (splitOn ",") . lines <$> readFile "inputs/3.txt"
    let firstWirePath = concat $ createPath (head input) (0, 0)
    let secondWirePath = concat $ createPath (last input) (0, 0)
    print $ minimum $ map centerManhattanDistance $ intersect firstWirePath secondWirePath

createPath :: [String] -> Point -> [[Point]]
createPath [] _ = []
createPath ((direction:len):xs) (x, y)
    | direction == 'R' = map (\n -> (x + n, y)) [1..lenNum] : createPath xs (x + lenNum, y)
    | direction == 'L' = map (\n -> (x - n, y)) [1..lenNum] : createPath xs (x - lenNum, y)
    | direction == 'U' = map (\n -> (x, y + n)) [1..lenNum] : createPath xs (x, y + lenNum)
    | direction == 'D' = map (\n -> (x, y - n)) [1..lenNum] : createPath xs (x, y - lenNum)
    where lenNum = read len

centerManhattanDistance :: Point -> Int
centerManhattanDistance (x, y) = abs x + abs y
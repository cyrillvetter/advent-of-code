import Data.Maybe (mapMaybe)
import qualified Data.Map as M

neighbours = [-1, 0, 1]

type Coord = (Int, Int)
data Dir = R | U | L | D deriving (Enum)

main = do
    input <- read <$> readFile "inputs/3.txt"
    print $ part1 input
    print $ part2 input

part1 :: Int -> Int
part1 num = calcDistance (generateCoordinates !! (num - 2))

part2 :: Int -> Int
part2 num = go generateCoordinates $ M.singleton (0, 0) 1
    where
        go :: [Coord] -> M.Map Coord Int -> Int
        go (c:cs) grid
            | neighbourSums > num = neighbourSums
            | otherwise = go cs (M.insert c neighbourSums grid)
            where neighbourSums = sum $ mapMaybe (`M.lookup` grid) $ getNeighbours c

calcDistance :: Coord -> Int
calcDistance (x, y) = abs x + abs y

generateCoordinates :: [Coord]
generateCoordinates = gen (generateMoves 1) (0, 0) R
    where
        gen :: [Int] -> Coord -> Dir -> [Coord]
        gen (n:ns) c dir = coords ++ gen ns (last coords) (nextDir dir)
            where coords = take n $ iterate (moveCoordinate dir) $ moveCoordinate dir c

moveCoordinate :: Dir -> Coord -> Coord
moveCoordinate R (x, y) = (x + 1, y)
moveCoordinate U (x, y) = (x, y + 1)
moveCoordinate L (x, y) = (x - 1, y)
moveCoordinate D (x, y) = (x, y - 1)

generateMoves :: Int -> [Int]
generateMoves n = n : n : generateMoves (n + 1)

nextDir :: Dir -> Dir
nextDir D = R
nextDir d = succ d

getNeighbours :: Coord -> [Coord]
getNeighbours (x, y) = [(x + i, y + j) | i <- neighbours, j <- neighbours, (i, j) /= (0, 0)]
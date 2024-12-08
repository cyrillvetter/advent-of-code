import Data.List (sortOn, groupBy, tails, nub)
import Data.Ix (Ix, inRange)

type Point = (Int, Int)

main = do
    input <- lines <$> readFile "inputs/8.txt"
    let antennas = groupAntennas $ createGrid input
        bound = ((0, 0), (length input - 1, length input - 1))

    print $ solve bound placeTwoAntinodes antennas
    print $ solve bound (placeAntinodesToBound bound) antennas

solve :: (Point, Point) -> ((Point, Point) -> [Point]) -> [[Point]] -> Int
solve bound placementFunc = length . nub . concatMap (concatMap (filter (inRange bound) . placementFunc) . pairs)

placeTwoAntinodes :: (Point, Point) -> [Point]
placeTwoAntinodes ((y1, x1), (y2, x2)) = [(y1 + (y1 - y2), x1 + (x1 - x2)), (y2 + (y2 - y1), x2 + (x2 - x1))]

placeAntinodesToBound :: (Point, Point) -> (Point, Point) -> [Point]
placeAntinodesToBound bound (a1, a2) = nub (takeWhileInBound addTuple ++ takeWhileInBound subTuple)
    where diff = subTuple a1 a2
          takeWhileInBound f = takeWhile (inRange bound) $ iterate (f diff) a1

groupAntennas :: [(Point, Char)] -> [[Point]]
groupAntennas = map (map fst) . groupBy (\(_, x) (_, y) -> x == y) . sortOn snd . filter ((/= '.') . snd)

subTuple :: Num a => (a, a) -> (a, a) -> (a, a)
subTuple (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

addTuple :: Num a => (a, a) -> (a, a) -> (a, a)
addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

createGrid :: [[Char]] -> [(Point, Char)]
createGrid xs = [((x, y), c) | (y, row) <- zip [0..] xs, (x, c) <- zip [0..] row]

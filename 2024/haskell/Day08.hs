import Data.List (sortOn, groupBy, tails)
import Data.Ix (inRange)
import qualified Data.Set as S

type Range = ((Int, Int), (Int, Int))
type Vec2 = (Int, Int)

main = do
    input <- lines <$> readFile "inputs/8.txt"
    let antennaPairs = map pairs $ groupAntennas $ createGrid input
        dimension = length input - 1
        bound = ((0, 0), (dimension, dimension))

    print $ solve (placeTwoAntinodes bound) antennaPairs
    print $ solve (placeAntinodesInBound bound) antennaPairs

solve :: ((Vec2, Vec2) -> [Vec2]) -> [[(Vec2, Vec2)]] -> Int
solve f = S.size . S.fromList . concatMap (concatMap f)

placeTwoAntinodes :: Range -> (Vec2, Vec2) -> [Vec2]
placeTwoAntinodes bound (a1, a2) = filter (inRange bound) [a1 `add` (a2 `sub` a1), a2 `add` (a1 `sub` a2)]

placeAntinodesInBound :: Range -> (Vec2, Vec2) -> [Vec2]
placeAntinodesInBound bound (a1, a2) = place (a2 `sub` a1) a1 ++ place (a1 `sub` a2) a2
    where
        place :: Vec2 -> Vec2 -> [Vec2]
        place diff = takeWhile (inRange bound) . iterate (add diff)

groupAntennas :: [(Vec2, Char)] -> [[Vec2]]
groupAntennas = map (map fst) . groupBy (\(_, x) (_, y) -> x == y) . sortOn snd . filter ((/= '.') . snd)

sub :: Vec2 -> Vec2 -> Vec2
sub (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

add :: Vec2 -> Vec2 -> Vec2
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

createGrid :: [[Char]] -> [(Vec2, Char)]
createGrid xs = [((x, y), c) | (y, row) <- zip [0..] xs, (x, c) <- zip [0..] row]
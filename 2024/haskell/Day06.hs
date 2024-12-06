import qualified Data.Array.Unboxed as A
import qualified Data.Set as S
import Common (count)

type Point = (Int, Int)
type Grid = A.UArray Point Char

main = do
    input <- lines <$> readFile "inputs/6.txt"
    let bound = length input - 1
        grid = A.listArray ((0, 0), (bound, bound)) $ concat input
        start = fst $ head $ filter ((== '^') . snd) $ A.assocs grid
        visitedPoints = getVisitedPoints grid start

    print $ S.size visitedPoints
    print $ count (isLoop start) $ placeObstacles grid visitedPoints

getVisitedPoints :: Grid -> Point -> S.Set Point
getVisitedPoints grid start = walk (S.singleton start) start (-1, 0)
    where
        walk :: S.Set Point -> Point -> Point -> S.Set Point
        walk visited p@(y, x) d@(dy, dx) = case grid A.!? next of
            Just '.' -> walk nextVisited next d
            Just '^' -> walk nextVisited next d
            Just '#' -> walk visited p (turnRight d)
            Nothing  -> visited
            where next = (y + dy, x + dx)
                  nextVisited = next `S.insert` visited

isLoop :: Point -> Grid -> Bool
isLoop start grid = walk S.empty start (-1, 0)
    where
        walk :: S.Set (Point, Point) -> Point -> Point -> Bool
        walk visited p@(y, x) d@(dy, dx)
            | (p, d) `S.member` visited = True
            | otherwise = case grid A.!? next of
                Just '.' -> walk nextVisited next d
                Just '^' -> walk nextVisited next d
                Just '#' -> walk visited p (turnRight d)
                Nothing  -> False
            where next = (y + dy, x + dx)
                  nextVisited = (p, d) `S.insert` visited

placeObstacles :: Grid -> S.Set Point -> [Grid]
placeObstacles grid = map (\p -> grid A.// [(p, '#')]) . S.elems

turnRight :: Point -> Point
turnRight p = case p of
    (-1, 0) ->  (0, 1)
    (0, 1) -> (1, 0)
    (1, 0) -> (0, -1)
    (0, -1) ->  (-1, 0)

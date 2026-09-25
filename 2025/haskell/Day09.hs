import Common (toTuple, adj4)
import Data.List.Split (splitOn)
import qualified Data.Set as S

import Debug.Trace (traceShow, trace)

type Tile = (Int, Int)

main = do
    input <- parse <$> readFile "inputs/09.txt"
    print $ maximum $ map area $ cartesianProduct input

    let loop = buildLoop (last input:input)
        bound = upperBound loop
    print bound
    print $ floodFillOuter [(0, 0)] bound S.empty

buildLoop :: [Tile] -> S.Set Tile
buildLoop (t1:t2:ts) = S.union (S.fromList connected) $ buildLoop (t2:ts)
    where connected = connectTiles t1 t2
buildLoop _ = S.empty

connectTiles :: Tile -> Tile -> [Tile]
connectTiles (x1, y1) (x2, y2) = if x1 == x2
    then map (x1,) $ range y1 y2
    else map (,y1) $ range x1 x2 
    where range x y = [min x y..max x y]

floodFillOuter :: [Tile] -> Tile -> S.Set Tile -> S.Set Tile
floodFillOuter [] _ visited = visited
floodFillOuter (p:ps) upperBound visited
    | traceShow p p `S.member` visited = floodFillOuter ps upperBound visited
    | otherwise = floodFillOuter (ps ++ n) upperBound (S.insert p visited)
    where n = filter (\t -> t >= (0, 0) && t <= upperBound && t `S.notMember` visited) $ adj4 p

upperBound :: S.Set Tile -> Tile
upperBound ts = (maxOf fst, maxOf snd)
    where maxOf f = (+ 1) $ maximum $ S.map f ts

cartesianProduct :: Eq a => [a] -> [(a, a)]
cartesianProduct xs = [(a, b) | a <- xs, b <- xs, a /= b]

area :: (Tile, Tile) -> Int
area ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

parse :: String -> [Tile]
parse = map (toTuple . map read . splitOn ",") . lines

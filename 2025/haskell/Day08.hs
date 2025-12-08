import Common (toTuple3)
import Data.List (sortOn, nub, partition)
import Data.Ord (Down(..))
import Data.List.Split (splitOn)
import qualified Data.Set as S

type Coord = (Int, Int, Int)

main = do
    input <- parse <$> readFile "inputs/08.txt"
    let pairs = closestPairs input
        circuits = map S.singleton input
        sizes = map S.size $ foldl connectCircuits circuits $ take 1000 pairs
    print $ product $ take 3 $ sortOn Down sizes
    print $ connectingPair circuits pairs

connectingPair :: [S.Set Coord] -> [(Coord, Coord)] -> Int
connectingPair circuits (p:ps) = case connectCircuits circuits p of
    [_] -> let ((x1, _, _), (x2, _, _)) = p in x1 * x2
    r  -> connectingPair r ps

connectCircuits :: [S.Set Coord] -> (Coord, Coord) -> [S.Set Coord]
connectCircuits circuits (c1, c2) = case with of
    [b] -> b : without
    [b1, b2] -> b1 `S.union` b2 : without
    where (with, without) = partition (\s -> c1 `S.member` s || c2 `S.member` s) circuits

closestPairs :: [Coord] -> [(Coord, Coord)]
closestPairs cs = nub $ sortOn dist $ [c | a <- cs, b <- cs, a /= b, let c = (min a b, max a b)]

dist :: (Coord, Coord) -> Double
dist ((x1, y1, z1), (x2, y2, z2)) = sqrt (fromIntegral ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2))

parse :: String -> [Coord]
parse = map (toTuple3 . map read . splitOn ",") . lines

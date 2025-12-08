import Common (toTuple3)
import Data.List (sortOn, sortBy, nub, partition)
import Data.List.Split (splitOn)
import qualified Data.Set as S

type Coord = (Int, Int, Int)

main = do
    input <- parse <$> readFile "inputs/08.txt"
    let pairs = closestPairs input
        circuits = map S.singleton input
    print $ product $ take 3 $ sortBy (flip compare) $ map S.size $ foldl connectCircuits circuits $ take 10 pairs

connectCircuits :: [S.Set Coord] -> (Coord, Coord) -> [S.Set Coord]
connectCircuits circuits (c1, c2) = case has of
    [b] -> b : hasNot
    [b1, b2] -> b1 `S.union` b2 : hasNot
    where (has, hasNot) = partition (\s -> c1 `S.member` s || c2 `S.member` s) circuits

closestPairs :: [Coord] -> [(Coord, Coord)]
closestPairs cs = nub $ sortOn dist $ [c | a <- cs, b <- cs, a /= b, let c = (min a b, max a b)]

dist :: (Coord, Coord) -> Double
dist ((x1, y1, z1), (x2, y2, z2)) = sqrt (fromIntegral ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2))

parse :: String -> [Coord]
parse = map (toTuple3 . map read . splitOn ",") . lines

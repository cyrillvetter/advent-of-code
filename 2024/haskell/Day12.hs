import qualified Data.Array.Unboxed as A
import qualified Data.Set as S
import Common (buildCharArray, adj4, subTuples)

type Point = (Int, Int)
type Grid = A.UArray Point Char

main = do
    grid <- buildCharArray <$> readFile "inputs/12.txt"
    print $ solve grid (A.indices grid) S.empty

solve :: Grid -> [Point] -> S.Set Point -> Int
solve grid [] _ = 0
solve grid (p:ps) visited = area * perim + solve grid ps nextVisited
    where (area, perim, nextVisited) = fillGardenRegion grid [p] visited

fillGardenRegion :: Grid -> [Point] -> S.Set Point -> (Int, Int, S.Set Point)
fillGardenRegion _ [] visited = (0, 0, visited)
fillGardenRegion grid (p:ps) visited
    | p `S.member` visited = fillGardenRegion grid ps visited
    | otherwise =
        let (area, perim, nextVisited) = fillGardenRegion grid (adj ++ ps) (p `S.insert` visited)
        in (area + 1, perim + length outside, nextVisited)
    where plot = grid A.! p
          adj = [ np | np <- adj4 p, valid np, np `S.notMember` visited ]
          outside = [ np | np <- adj4 p, not (valid np) ]
          valid np = case grid A.!? np of
              Nothing -> False
              Just v  -> plot == v

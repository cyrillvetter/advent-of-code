import qualified Data.Array.Unboxed as A
import qualified Data.Set as S
import Common (buildCharArray, adj4, subTuples)

type Point = (Int, Int)
type Grid = A.UArray Point Char
type Region = (Char, [Point])

main = do
    grid <- buildCharArray <$> readFile "inputs/12.txt"
    let regions = getAllRegions grid (A.indices grid) S.empty
    print $ sum $ map (p1 grid) regions

-- area * perimeter
p1 :: Grid -> Region -> Int
p1 grid (plot, region) = length region * perim
    where perim = length $ concatMap (filter (not . insideRegion grid plot) . adj4) region

-- area * sides
p2 :: Grid -> Region -> Int
p2 grid (plot, region) = 0

getAllRegions :: Grid -> [Point] -> S.Set Point -> [Region]
getAllRegions _ [] _ = []
getAllRegions grid (p:ps) visited = case getRegion [p] visited of
    ([], vis) -> getAllRegions grid ps vis
    (region@(p:_), vis) -> (grid A.! p, region) : getAllRegions grid ps vis
    where
        getRegion :: [Point] -> S.Set Point -> ([Point], S.Set Point)
        getRegion [] visited = ([], visited)
        getRegion (p:ps) visited
            | p `S.member` visited = getRegion ps visited
            | otherwise = (p : others, nextVisited)
            where plot = grid A.! p
                  adj = filter (insideRegion grid plot) (adj4 p)
                  (others, nextVisited) = getRegion (adj ++ ps) (p `S.insert` visited)

insideRegion :: Grid -> Char -> Point -> Bool
insideRegion grid plot point = case grid A.!? point of
    Nothing -> False
    Just v  -> plot == v

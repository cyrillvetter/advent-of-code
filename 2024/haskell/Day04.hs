import qualified Data.Array.Unboxed as A
import Common (count)

type Point = (Int, Int)
type Grid = A.UArray Point Char

dirs = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]

main = do
    input <- lines <$> readFile "inputs/4.txt"
    let bound = length input - 1
        grid = A.listArray ((0, 0), (bound, bound)) $ concat input
        assocs = A.assocs grid

    print $ sum $ map (countXmas grid . fst) assocs
    print $ count (isCrossMas grid) $ map fst $ filter ((== 'A') . snd) assocs

countXmas :: Grid -> Point -> Int
countXmas grid p = count ((== "XMAS") . getRange grid) $ map (createPath p) dirs

isCrossMas :: Grid -> Point -> Bool
isCrossMas grid (y, x) = all ((`elem` ["MS", "SM"]) . getRange grid) [diag1, diag2]
    where diag1 = [(y - 1, x - 1), (y + 1, x + 1)]
          diag2 = [(y + 1, x - 1), (y - 1, x + 1)]

createPath :: Point -> Point -> [Point]
createPath (x, y) (dx, dy) = map (\i -> (x + dx * i, y + dy * i)) [0..3]

getRange :: Grid -> [Point] -> String
getRange grid = map (maybe '.' id . (grid A.!?))

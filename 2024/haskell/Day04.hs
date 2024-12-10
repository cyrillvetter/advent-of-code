import qualified Data.Array.Unboxed as A
import Common (count, buildCharArray)

type Point = (Int, Int)
type Grid = A.UArray Point Char

dirs = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]
cross = ["SM", "MS"]

main = do
    grid <- buildCharArray <$> readFile "inputs/4.txt"
    let assocs = A.assocs grid
    print $ sum $ map (countXmas grid . fst) assocs
    print $ count (isCrossMas grid) $ map fst $ filter ((== 'A') . snd) assocs

countXmas :: Grid -> Point -> Int
countXmas grid p = count ((== "XMAS") . getRange grid) $ map (createPath p) dirs

isCrossMas :: Grid -> Point -> Bool
isCrossMas grid (y, x) = diag1 `elem` cross && diag2 `elem` cross
    where diag1 = getRange grid [(y - 1, x - 1), (y + 1, x + 1)]
          diag2 = getRange grid [(y + 1, x - 1), (y - 1, x + 1)]

createPath :: Point -> Point -> [Point]
createPath (x, y) (dx, dy) = [ (x + dx * i, y + dy * i) | i <- [0..3] ]

getRange :: Grid -> [Point] -> String
getRange grid = map (maybe '.' id . (grid A.!?))

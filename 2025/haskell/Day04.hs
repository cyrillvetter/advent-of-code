import Common (buildCharArray, adj8, count)
import qualified Data.Array.Unboxed as A

type Coord = (Int, Int)
type Grid = A.UArray Coord Char

main = do
    grid <- buildCharArray <$> readFile "inputs/04.txt"
    print $ count (accessible grid) [ c | (c, '@') <- A.assocs grid ]
    print $ remove grid

remove :: Grid -> Int
remove grid = case updates of
    [] -> 0
    _ -> length updates + remove (grid A.// updates)
    where updates = [ (c, '.') | (c, '@') <- A.assocs grid, accessible grid c ]

accessible :: Grid -> Coord -> Bool
accessible grid c = count ((== Just '@') . (grid A.!?)) (adj8 c) < 4

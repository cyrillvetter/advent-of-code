import Data.List (nub)
import qualified Data.Array.Unboxed as A
import Common (adj4, buildCharArray)

type Point = (Int, Int)
type Grid = A.UArray Point Char

main = do
    grid <- buildCharArray <$> readFile "inputs/10.txt"
    let targets = [ trails grid '0' p | (p, '0') <- A.assocs grid ]
    print $ sum $ map (length . nub) targets
    print $ sum $ map length targets

trails :: Grid -> Char -> Point -> [Point]
trails _ '9' p = [p]
trails grid c p = concat [ trails grid (grid A.! adj) adj | adj <- adj4 p, valid adj]
    where valid p = case grid A.!? p of
              Just n  -> succ c == n
              Nothing -> False

import Common (toTuple)
import Data.List (sortOn)
import Data.List.Split (splitOn)

type Range = (Int, Int)

main = do
    (ranges, ids) <- parse <$> readFile "inputs/05.txt"
    print $ length $ filter (\i -> any (inRange i) ranges) ids
    print $ combineRanges $ sortOn fst ranges

inRange :: Int -> Range -> Bool
inRange x (l, u) = x >= l && x <= u

combineRanges :: [Range] -> Int
combineRanges [(l, u)] = u - l + 1
combineRanges ((l1, u1):(l2, u2):rs)
    | u1 < l1 = combineRanges ((max l1 l2, u2):rs)
    | otherwise = u1 - l1 + 1 + combineRanges ((max (u1 + 1) l2, u2):rs)

parse :: String -> ([Range], [Int])
parse input = (ranges, map read available)
    where [fresh, available] = map lines $ splitOn "\n\n" input
          ranges = map (toTuple . map read . splitOn "-") fresh

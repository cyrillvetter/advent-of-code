import Common (toTuple)
import Data.List.Split (splitOn)

type Range = (Int, Int)

main = do
    (ranges, ids) <- parse <$> readFile "inputs/05.txt"
    print $ length $ filter (\i -> any (inRange i) ranges) ids

inRange :: Int -> Range -> Bool
inRange x (l, u) = x >= l && x <= u

parse :: String -> ([Range], [Int])
parse input = (ranges, map read available)
    where [fresh, available] = map lines $ splitOn "\n\n" input
          ranges = map (toTuple . map read . splitOn "-") fresh

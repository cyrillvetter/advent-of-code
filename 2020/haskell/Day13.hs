import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

main = do
    [p1, p2] <- lines <$> readFile "inputs/13.txt"
    let departure = read p1
        nums = splitOn "," p2
    print $ part1 departure nums

part1 :: Int -> [String] -> Int
part1 departure ids = id * (time - departure)
    where nums = map read $ filter (/= "x") ids
          (id, time) = minimumBy (compare `on` snd) $ map (\n -> (n, calcEarliestTime departure n)) nums

calcEarliestTime :: Int -> Int -> Int
calcEarliestTime departure id = id * ceiling (fromIntegral departure / fromIntegral id)

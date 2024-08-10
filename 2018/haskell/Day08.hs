import Debug.Trace (trace)
import Data.List.Split (chunksOf)

main = do
    input <- map read . words <$> readFile "inputs/8.txt"
    print $ calcMetadataSum input
    print "Day 8"

calcMetadataSum :: [Int] -> Int
calcMetadataSum (c:m:xs) = sum metadata + calcMetadataSum xs
    where len = length xs
          (next, metadata) = splitAt (len - m) xs

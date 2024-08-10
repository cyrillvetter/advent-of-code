import Data.List (sort, intersect, elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

alph = ['a'..'z'] ++ ['A'..'Z']

main = do
    input <- readFile "inputs/3.txt"
    print $ sum $ map (getPriority . headIntersect . splitHalf) $ lines input
    print $ sum $ map (getPriority . headIntersect) $ chunksOf 3 $ lines input

getPriority :: Char -> Int
getPriority c = 1 + fromJust (c `elemIndex` alph)

splitHalf :: String -> [String]
splitHalf line = [fst split, snd split]
    where split = splitAt (length line `div` 2) line

headIntersect :: [String] -> Char
headIntersect = head . foldr1 intersect
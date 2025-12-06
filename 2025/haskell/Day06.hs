import Data.List (transpose)
import Data.List.Split (splitWhen)

main = do
    input <- lines <$> readFile "inputs/06.txt"
    let ops = map op $ words $ last input
        solve p = sum $ zipWith foldl1 ops $ p $ init input
    print $ solve p1
    print $ solve p2

op :: String -> (Int -> Int -> Int)
op "*" = (*)
op "+" = (+)

p1 :: [String] -> [[Int]]
p1 = transpose . map (map read . words)

p2 :: [String] -> [[Int]]
p2 = map (map read) . splitWhen (all (== ' ')) . transpose

import Data.List.Split (splitOn)

main = do
    input <- parse <$> readFile "inputs/4.txt"
    print $ length . filter (\[l, r] -> fullyContain l r) $ input
    print $ length . filter (\[l, r] -> overlap l r) $ input

parse :: String -> [[[Int]]]
parse = map (map (map (read @Int) . splitOn "-") . splitOn ",") . lines

fullyContain :: [Int] -> [Int] -> Bool
fullyContain [a1, a2] [b1, b2] = a1 >= b1 && a2 <= b2 || b1 >= a1 && b2 <= a2

overlap :: [Int] -> [Int] -> Bool
overlap [a1, a2] [b1, b2] = a1 <= b2 && b1 <= a2
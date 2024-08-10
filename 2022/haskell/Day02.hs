import Data.List.Split (splitOn)

main = do
    input <- readFile "inputs/2.txt"
    print . sum . map countScore $ parse input
    print . sum . map resultScore $ parse input

parse :: String -> [(Int, Int)]
parse = map (playToScores . splitOn " ") . lines

playToScores :: [String] -> (Int, Int)
playToScores [x, y] = (shapeScore $ head x, shapeScore $ head y)

countScore :: (Int, Int) -> Int
countScore (a, b)
    | a == b = b + 3
    | a == 1 && b == 2 || a == 2 && b == 3 || a == 3 && b == 1 = b + 6
    | otherwise = b

resultScore :: (Int, Int) -> Int
resultScore (a, b)
    | b == 1 = if a > 1 then a - 1 else 3
    | b == 2 = a + 3
    | otherwise = 6 + if a < 3 then a + 1 else 1

shapeScore :: Char -> Int
shapeScore c
    | c == 'A' || c == 'X' = 1
    | c == 'B' || c == 'Y' = 2
    | c == 'C' || c == 'Z' = 3
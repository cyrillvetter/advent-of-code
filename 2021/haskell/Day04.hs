import Data.List.Split (splitOn)

type Board = [[Maybe Int]]

main = do
    (numbers, boards) <- parse <$> readFile "inputs/4.txt"
    print boards

markBoards :: [Int] -> [Board] -> Int
markBoards (x:xs) boards = undefined

markNum :: Int -> Maybe Int -> Maybe Int
markNum _ Nothing = Nothing
markNum exp (Just x)
    | exp == x = Nothing
    | otherwise = Just x

parse :: String -> ([Int], [Board])
parse input = (numbers, boards)
    where (p1:p2) = splitOn "\n\n" input
          numbers = map read $ splitOn "," p1
          boards = map (map (map (Just . read) . words) . lines) p2

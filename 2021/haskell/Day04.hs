import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.List (transpose)

type Board = [[Maybe Int]]

main = do
    (numbers, boards) <- parse <$> readFile "inputs/4.txt"
    print $ part1 numbers boards
    print $ part2 numbers boards

part1 :: [Int] -> [Board] -> Int
part1 (x:xs) boards = case winners of
    (winner:_) -> x * getUnmarkedSum winner
    _          -> part1 xs marked
    where marked = map (markBoard x) boards
          winners = filter isWinner marked

part2 :: [Int] -> [Board] -> Int
part2 (x:xs) [last]
    | isWinner marked = x * getUnmarkedSum marked
    | otherwise = part2 xs [marked]
    where marked = markBoard x last
part2 (x:xs) boards = part2 xs losers
    where losers = filter (not . isWinner) $ map (markBoard x) boards

isWinner :: Board -> Bool
isWinner board = wins board || wins (transpose board)
    where
        wins :: Board -> Bool
        wins = any (all (== Nothing))

getUnmarkedSum :: Board -> Int
getUnmarkedSum = sum . catMaybes . concat

markBoard :: Int -> Board -> Board
markBoard x = map (map (markNum x))

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

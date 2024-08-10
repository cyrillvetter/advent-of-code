import Data.List.Split (splitOn)
import Data.List (transpose)
import Data.Char (isDigit, isAlpha)
import Prelude hiding (Right)

type Grid = [String]
type Instructions = [String]
data Direction = Right | Down | Left | Up deriving (Eq, Enum)

main = do
    input <- splitOn "\n\n" <$> readFile "inputs/22.txt"
    let grid = lines $ head input
    let transposedGrid = transpose grid
    let instructions = parse $ last input
    putStrLn "Day 22"

trail :: Grid -> Grid -> Instructions -> (Int, Int) -> Direction -> Int
trail grid transposedGrid (instr:xs) (x, y) dir
    | isAlpha $ head instr = trail grid transposedGrid xs (x, y) (rotate dir instr)
    | otherwise = 0

rotate :: Direction -> String -> Direction
rotate dir r
    | r == "R" && dir == Up = Right
    | r == "L" && dir == Right = Up
    | r == "R" = succ dir
    | otherwise = pred dir

parse :: String -> Instructions
parse [] = []
parse s
    | isAlpha $ head s = [head s] : parse (tail s)
    | otherwise =
        let untilLetter = takeWhile isDigit s
            len = length untilLetter
        in untilLetter : parse (drop len s)
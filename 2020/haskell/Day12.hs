type Point = (Int, Int)
type Instruction = (Char, Int)
data Direction = N | E | S | W deriving (Enum, Eq, Show)

main = do
    input <- map parseLine . lines <$> readFile "inputs/12.txt"
    print $ part1 input (0, 0) E

part1 :: [Instruction] -> Point -> Direction -> Int
part1 (('N', val):xs) (x, y) d = part1 xs (x, y + val) d
part1 (('S', val):xs) (x, y) d = part1 xs (x, y - val) d
part1 (('E', val):xs) (x, y) d = part1 xs (x + val, y) d
part1 (('W', val):xs) (x, y) d = part1 xs (x - val, y) d
part1 (('L', val):xs) p d = part1 xs p (iterN rotL d (val `div` 90))
part1 (('R', val):xs) p d = part1 xs p (iterN rotR d (val `div` 90))
part1 (('F', val):xs) p d = part1 ((dirToChar d, val):xs) p d
part1 [] (x, y) _ = abs x + abs y

iterN :: (a -> a) -> a -> Int -> a
iterN f x n = iterate f x !! n

rotR :: Direction -> Direction
rotR W = N
rotR d = succ d

rotL :: Direction -> Direction
rotL N = W
rotL d = pred d

dirToChar :: Direction -> Char
dirToChar dir = case dir of
    N -> 'N'
    E -> 'E'
    S -> 'S'
    W -> 'W'

parseLine :: String -> Instruction
parseLine (x:xs) = (x, read xs)
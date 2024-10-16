data Direction = N | E | S | W deriving (Enum, Eq, Show)

type Point = (Int, Int)
type Ship = (Point, Direction)
type WaypointShip = (Point, Point)
type Instruction = (Char, Int)

main = do
    input <- map parseLine . lines <$> readFile "inputs/12.txt"
    print $ dist $ foldl part1 ((0, 0), E) input
    print $ dist $ foldl part2 ((0, 0), (10, 1)) input

part1 :: Ship -> Instruction -> Ship
part1 ((x, y), d) ('N', val) = ((x, y + val), d)
part1 ((x, y), d) ('S', val) = ((x, y - val), d)
part1 ((x, y), d) ('E', val) = ((x + val, y), d)
part1 ((x, y), d) ('W', val) = ((x - val, y), d)
part1 (p, d) ('L', val) = (p, iterN prev d (val `div` 90))
part1 (p, d) ('R', val) = (p, iterN next d (val `div` 90))
part1 (p, d) ('F', val) = part1 (p, d) (dirToChar d, val)

part2 :: WaypointShip -> Instruction -> WaypointShip
part2 (s, (x, y)) ('N', val) = (s, (x, y + val))
part2 (s, (x, y)) ('S', val) = (s, (x, y - val))
part2 (s, (x, y)) ('E', val) = (s, (x + val, y))
part2 (s, (x, y)) ('W', val) = (s, (x - val, y))
part2 (p, d) ('L', val) = (p, rotate d val)
part2 (p, d) ('R', val) = (p, rotate d (val * (-1)))
part2 ((sx, sy), (wx, wy)) ('F', val) = ((sx + val * wx, sy + val * wy), (wx, wy))

iterN :: (a -> a) -> a -> Int -> a
iterN f x n = iterate f x !! n

next :: Direction -> Direction
next W = N
next d = succ d

prev :: Direction -> Direction
prev N = W
prev d = pred d

dirToChar :: Direction -> Char
dirToChar dir = case dir of
    N -> 'N'
    E -> 'E'
    S -> 'S'
    W -> 'W'

rotate :: Point -> Int -> Point
rotate (x, y) deg = ((x * intCos deg) - (y * intSin deg), (y * intCos deg) + (x * intSin deg))

intCos :: Int -> Int
intCos = round . cos . deg2rad . fromIntegral

intSin :: Int -> Int
intSin = round . sin . deg2rad . fromIntegral

deg2rad :: Floating a => a -> a
deg2rad deg = deg * (pi / 180)

dist :: (Point, a) -> Int
dist ((x, y), _) = abs x + abs y

parseLine :: String -> Instruction
parseLine (x:xs) = (x, read xs)
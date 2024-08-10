import Data.Set as S

type House = (Int, Int)

main = do
    input <- readFile "inputs/3.txt"
    let origin = (0, 0)
    print $ countVisitedHouses input (S.singleton origin) origin
    print $ countVisitedHouses' input (S.singleton origin) True origin origin

countVisitedHouses :: String -> S.Set House -> House -> Int
countVisitedHouses [] visited _ = S.size visited
countVisitedHouses (h:hs) visited current = countVisitedHouses hs (S.insert nextHouse visited) nextHouse
    where nextHouse = move h current

countVisitedHouses' :: String -> S.Set House -> Bool -> House -> House -> Int
countVisitedHouses' [] visited _ _ _ = S.size visited
countVisitedHouses' (h:hs) visited isSanta santa robot
    | isSanta = countVisitedHouses' hs (S.insert nextSanta visited) (not isSanta) nextSanta robot
    | otherwise = countVisitedHouses' hs (S.insert nextRobot visited) (not isSanta) santa nextRobot
    where nextSanta = move h santa
          nextRobot = move h robot

move :: Char -> House -> House
move d (x, y)
    | d == '>' = (x + 1, y)
    | d == 'v' = (x, y - 1)
    | d == '<' = (x - 1, y)
    | otherwise = (x, y + 1)

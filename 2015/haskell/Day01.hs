main = do
    input <- readFile "inputs/1.txt"
    print $ getFloor input
    print $ getFirstBasementIndex input 0 0

getFloor :: String -> Int
getFloor [] = 0
getFloor (x:xs)
    | x == '(' = 1 + getFloor xs
    | otherwise = getFloor xs - 1

getFirstBasementIndex :: String -> Int -> Int -> Int
getFirstBasementIndex _ i (-1) = i
getFirstBasementIndex (x:xs) i floor = getFirstBasementIndex xs (i + 1) (floor + move)
    where move = if x == '(' then 1 else (-1)

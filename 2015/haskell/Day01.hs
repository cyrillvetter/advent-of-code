import Data.List (elemIndex)

main = do
    input <- map move <$> readFile "inputs/1.txt"
    print $ sum input
    print $ maybe undefined (+ 1) $ elemIndex (-1) $ scanl1 (+) input

move :: Char -> Int
move '(' = 1
move ')' = -1

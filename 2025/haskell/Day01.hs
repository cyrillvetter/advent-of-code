dialStart = 50

main = do
    input <- map parse . lines <$> readFile "inputs/01.txt"
    print $ length $ filter (== 0) $ scanl turnDial dialStart input
    print $ snd $ foldl countClicks (dialStart, 0) input

turnDial :: Int -> Int -> Int
turnDial num amt = (num + amt) `mod` 100

countClicks :: (Int, Int) -> Int -> (Int, Int)
countClicks (num, acc) amt = (turnDial num amt, acc + clicks)
    where moves = replicate (abs amt - 1) (signum amt)
          clicks = length $ filter (== 0) $ scanl turnDial num moves

parse :: String -> Int
parse ('L':n) = -(read n)
parse ('R':n) = read n

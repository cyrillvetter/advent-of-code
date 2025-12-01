dialStart = 50

main = do
    input <- map parse . lines <$> readFile "inputs/01.txt"
    print $ length $ filter (== 0) $ scanl p1 dialStart input
    print $ snd $ foldl p2 (dialStart, 0) input

p1 :: Int -> Int -> Int
p1 cur turn = (if next < 0 then 100 - abs next else next) `mod` 100
    where next = cur + turn

p2 :: (Int, Int) -> Int -> (Int, Int)
p2 (cur, acc) turn = (p1 cur turn, acc + res)
    where res = length $ filter (== 0) $ drop 1 $ scanl (\acc c -> if (acc + c) < 0 || (acc + c) > 99 then 100 - abs (acc + c) else acc + c) cur $ replicate (abs turn) (signum turn)

parse :: String -> Int
parse ('L':n) = -(read n)
parse ('R':n) = read n

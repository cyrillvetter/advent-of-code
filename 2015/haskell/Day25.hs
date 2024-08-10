row = 3010
column = 3019

first = 20151125
mulBy = 252533
divBy = 33554393

main = do
    print $ first * powMod mulBy calculateSequencePos divBy `mod` divBy

calculateSequencePos :: Int
calculateSequencePos = seqVal - 1
    where colVal = scanl (+) 0 [1..] !! column
          seqVal = scanl (+) colVal [column..] !! (row - 1)

powMod :: Int -> Int -> Int -> Int
powMod b 0 m = 1 `mod` m
powMod b e m
    | even e = (powMod b (e `div` 2) m ^ 2) `mod` m
    | otherwise = (b * powMod b (e - 1) m) `mod` m

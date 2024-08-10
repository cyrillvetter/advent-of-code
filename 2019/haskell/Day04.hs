import Data.List (group)

lower = 264793
upper = 803935

main = do
    let increasingDigitPasswords = map (map length) $ filter allIncrease $ map (group . digits) [lower..upper]
    print $ length $ filter (any (>= 2)) increasingDigitPasswords
    print $ length $ filter (elem 2) increasingDigitPasswords

digits :: Integral a => a -> [a]
digits 0 = []
digits n = mod : digits div
    where (div, mod) = n `divMod` 10

allIncrease :: Integral a => [[a]] -> Bool
allIncrease [_] = True
allIncrease (x:xs)
    | head x < head (head xs) = False
    | otherwise = allIncrease xs

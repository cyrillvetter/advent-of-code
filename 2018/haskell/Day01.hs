import qualified Data.IntSet as IS

main = do
    input <- map convert . lines <$> readFile "inputs/1.txt"
    print $ sum input
    print $ firstDuplicate $ scanl1 (+) $ cycle input

convert :: [Char] -> Int
convert (x:xs) = if x == '-' then read $ x:xs else read xs

firstDuplicate :: [Int] -> Int
firstDuplicate = check IS.empty
    where
        check :: IS.IntSet -> [Int] -> Int
        check set (x:xs)
            | x `IS.member` set = x
            | otherwise = check (x `IS.insert` set) xs
import Data.List (nub)
import Data.List.Split (splitOn, chunksOf)

main = do
    input <- genIds <$> readFile "inputs/02.txt"
    print $ sum $ filter repeatsTwice input
    print $ sum $ filter repeats input

repeatsTwice :: Int -> Bool
repeatsTwice num = uncurry (==) $ splitAt (length id `div` 2) id
    where id = show num

repeats :: Int -> Bool
repeats num = any (((== 1) . length) . nub) chunks
    where id = show num
          lengths = [1..(length id `div` 2)]
          chunks = map (`chunksOf` id) lengths

genIds :: String -> [Int]
genIds = concatMap ((\[a, b] -> [a,(a+1)..b]) . map read . splitOn "-") . splitOn ","

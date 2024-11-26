import Data.List.Split (chunksOf)

main = do
    input <- map convert <$> readFile "inputs/16.txt"
    putStrLn $ calculate 272 input
    putStrLn $ calculate 35651584 input

calculate :: Int -> [Bool] -> String
calculate size = map convertBack . checksum . fillDisk size

checksum :: [Bool] -> [Bool]
checksum = head . dropWhile (even . length) . iterate (map checkPair . chunksOf 2)

checkPair :: [Bool] -> Bool
checkPair [x, y]
    | x == y = True
    | otherwise = False

fillDisk :: Int -> [Bool] -> [Bool]
fillDisk size = take size . head . dropWhile ((<= size) . length) . iterate process

process :: [Bool] -> [Bool]
process x = x ++ [False] ++ map not (reverse x)

convert :: Char -> Bool
convert '1' = True
convert '0' = False

convertBack :: Bool -> Char
convertBack True = '1'
convertBack False = '0'

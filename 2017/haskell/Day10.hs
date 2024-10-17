import Common (fst3)
import Data.List.Split (splitOn, chunksOf)
import Data.Char (ord)
import Numeric (showHex)
import Data.Bits (xor)

size = 256
codeSequence = [17, 31, 73, 47, 23]

main = do
    input <- readFile "inputs/10.txt"
    let lengths = map read $ splitOn "," input
        list = [0..(size - 1)]
    print $ product $ take 2 $ fst3 $ compute lengths list 0 0
    putStrLn $ part2 input

compute :: [Int] -> [Int] -> Int -> Int -> ([Int], Int, Int)
compute [] xs l s = (xs, l, s)
compute (x:xs) list pos skip = compute xs newList ((pos + x + skip) `mod` size) (skip + 1)
    where reversed = reverse $ take x $ drop pos $ cycle list
          remaining = take (size - x) $ drop (pos + x) $ cycle list
          newList = rotate (size - (pos `mod` size)) (reversed ++ remaining)

part2 :: String -> String
part2 input = concatMap toLeadingHex sparse
    where codes = map ord input ++ codeSequence
          codesSum = sum codes
          skipSum = sum [0..(length codes - 1)]
          newSize = size + length codeSequence
          (result, _, _) = foldl (\(res, l, s) c -> compute codes res l s) ([0..(size - 1)], 0, 0) [0..63]
          sparse = map (foldl1 xor) $ chunksOf 16 result

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop n (cycle xs)) xs

toLeadingHex :: Int -> String
toLeadingHex n = case hex of
    [_] -> '0' : hex
    _   -> hex
    where hex = showHex n ""

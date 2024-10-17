import Data.List.Split (splitOn)

size = 256

main = do
    input <- map read . splitOn "," <$> readFile "inputs/10.txt"
    let list = [0..(size - 1)]
    print $ part1 input list 0 0

part1 :: [Int] -> [Int] -> Int -> Int -> Int
part1 [] (x:y:xs) _ _ = x * y
part1 (x:xs) list pos skip = part1 xs newList (pos + x + skip) (skip + 1)
    where reversed = reverse $ take x $ drop pos $ cycle list
          remaining = take (size - x) $ drop (pos + x) $ cycle list
          newList = rotate (size - (pos `mod` size)) (reversed ++ remaining)

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop n (cycle xs)) xs

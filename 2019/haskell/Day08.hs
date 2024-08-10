import Data.List.Split (chunksOf)
import Data.List (minimumBy, transpose, intercalate)
import Data.Function (on)

main = do
    input <- readFile "inputs/8.txt"
    print $ snd $ minimumBy (compare `on` fst) $ map (`countDigits` (0, 0, 0)) $ chunksOf 150 input
    putStrLn $ intercalate "\n" $ chunksOf 25 $ map render $ transpose $ chunksOf 150 input

render :: String -> Char
render (x:xs)
    | x == '0' = ' '
    | x == '1' = '#'
    | otherwise = render xs

countDigits :: String -> (Int, Int, Int) -> (Int, Int)
countDigits [] (zero, one, two) = (zero, one * two)
countDigits (x:xs) (zero, one, two)
    | x == '0' = countDigits xs (zero + 1, one , two)
    | x == '1' = countDigits xs (zero, one + 1, two)
    | x == '2' = countDigits xs (zero, one, two + 1)

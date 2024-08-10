import Data.Char (isDigit, digitToInt, intToDigit)

main = do
    sumDecimals <- sum . map snafuToDecimal . lines <$> readFile "inputs/25.txt"
    print $ decimalToSnafu sumDecimals

snafuToDecimal :: String -> Int
snafuToDecimal [] = 0
snafuToDecimal (x:xs) = convertToInt x * (5 ^ length xs) + snafuToDecimal xs

decimalToSnafu :: Int -> String
decimalToSnafu 0 = ""
decimalToSnafu num =
    let remainder = num `mod` 5
        nextNum = num `div` 5 + (if remainder > 2 then 1 else 0)
    in decimalToSnafu nextNum ++ [convertToChar remainder]

convertToInt :: Char -> Int
convertToInt c
    | isDigit c = digitToInt c
    | c == '-' = -1
    | otherwise = -2

convertToChar :: Int -> Char
convertToChar i
    | i <= 2 = intToDigit i
    | i == 3 = '='
    | otherwise = '-'
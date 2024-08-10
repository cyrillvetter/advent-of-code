import Data.Char (digitToInt)
main = do
    grid <- lines <$> readFile "inputs/12.txt"
    print grid
    print "Day 12"
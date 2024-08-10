import Common (windowsOf, count)
import Data.List (intersect, isInfixOf)

main = do
    input <- lines <$> readFile "inputs/5.txt"
    print $ count part1 input
    print $ count part2 input

part1 :: String -> Bool
part1 l = threeVowels l && hasDoubleLetters l && notContainAny l

threeVowels :: String -> Bool
threeVowels = (>= 3) . length . (`intersect` "aeiou")

hasDoubleLetters :: String -> Bool
hasDoubleLetters = any (\[a, b] -> a == b) . windowsOf 2

notContainAny :: String -> Bool
notContainAny str = not $ any (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

part2 :: String -> Bool
part2 l = trapped l && repeatingPairs l

trapped :: String -> Bool
trapped = any (\[a, _, b] -> a == b) . windowsOf 3

repeatingPairs :: String -> Bool
repeatingPairs str = any (\(skip, from) -> from `isInfixOf` drop skip str) $ zip [2..] $ windowsOf 2 str

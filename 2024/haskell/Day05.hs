import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Common (toTuple)

type Rules = S.Set (Int, Int)

main = do
    [p1, p2] <- map lines . splitOn "\n\n" <$> readFile "inputs/5.txt"
    let rules = parseRules p1
        updates = map (map read . splitOn ",") p2

    print $ part1 rules updates
    print $ part2 rules updates

part1 :: Rules -> [[Int]] -> Int
part1 rules = sum . map getMiddle . filter (isSorted rules)

part2 :: Rules -> [[Int]] -> Int
part2 rules = sum . map (getMiddle . sortByRules rules) . filter (not . isSorted rules)

isSorted :: Rules -> [Int] -> Bool
isSorted rules xs = xs == sortByRules rules xs

sortByRules :: Rules -> [Int] -> [Int]
sortByRules rules = sortBy (rulesCmp rules)

rulesCmp :: Rules -> Int -> Int -> Ordering
rulesCmp rules x y = if (x, y) `S.member` rules then LT else GT

parseRules :: [String] -> Rules
parseRules = S.fromList . map (toTuple . map read . splitOn "|")

getMiddle :: [a] -> a
getMiddle xs = xs !! (length xs `div` 2)

import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Common (toTuple)

type Rules = S.Set (Int, Int)

main = do
    [p1, p2] <- map lines . splitOn "\n\n" <$> readFile "inputs/5.txt"
    let rules = parseRules p1
        updates = map (map read . splitOn ",") p2

    print $ process rules id updates
    print $ process rules not updates

process :: Rules -> (Bool -> Bool) -> [[Int]] -> Int
process rules sortF = sum . map (getMiddle . sortWithRules) . filter (sortF . isSorted)
    where rulesCmp x y = if (x, y) `S.member` rules then LT else GT
          sortWithRules = sortBy rulesCmp
          isSorted xs = xs == sortWithRules xs

parseRules :: [String] -> Rules
parseRules = S.fromList . map (toTuple . map read . splitOn "|")

getMiddle :: [a] -> a
getMiddle xs = xs !! (length xs `div` 2)

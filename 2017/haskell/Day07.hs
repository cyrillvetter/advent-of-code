import Data.List.Split (splitOn)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Map as M

type Program = (String, Int, [String])

main = do
    input <- map parseLine . lines <$> readFile "inputs/7.txt"
    let root = getRoot input
    print root
    print $ getUnbalance input root

getRoot :: [Program] -> String
getRoot p = head $ filter (`S.notMember` children) parents
    where children = S.fromList $ concatMap (\(_, _, s) -> s) p
          parents = map (\(s, _, _) -> s) p

getUnbalance :: [Program] -> String -> Int
getUnbalance p = find
    where parents = filter (\(_, _, s) -> not (null s)) p
          look = M.fromList $  map (\(a, b, c) -> (a, (b, c))) p
          find :: String -> Int
          find name
              | null children = weight
              | top /= bottom = trace (show result) result
              | otherwise = weight + sum childrenSums
              where (weight, children) = look M.! name
                    childrenSums = map find children
                    sortedChildren = groupBy (\(_, a) (_, b) -> a == b) $ sortBy (comparing snd) $ zip children childrenSums
                    top = snd (head (head sortedChildren))
                    bottom = snd (head (last sortedChildren))
                    result = fst (look M.! fst (head $ head $ filter (\g -> length g == 1) sortedChildren)) - abs (top - bottom)

parseLine :: String -> Program
parseLine line
    | length s == 1 = (name, weight, [])
    | otherwise = (name, weight, sub)
    where s = splitOn " -> " line
          leftSplit = words $ head s
          name = head leftSplit
          weight = read $ init $ tail $ last leftSplit
          sub = splitOn ", " $ last s

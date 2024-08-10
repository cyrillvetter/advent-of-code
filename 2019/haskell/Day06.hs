import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isNothing, fromJust)
import Data.Tuple (swap)

main = do
    -- part 1
    input <- readFile "inputs/6.txt"
    let parseInput = parsePath input
    let mapParsedInput = M.fromList parseInput
    print $ sum $ map (traceToCOM mapParsedInput) $ M.keys mapParsedInput

    -- part 2
    let orbitsInput = parseOrbits parseInput M.empty
    let combinedOrbits = parseOrbits (map swap parseInput) orbitsInput
    print $ traceToSAN combinedOrbits

traceToCOM :: M.Map String String -> String -> Int
traceToCOM _ "COM" = 0
traceToCOM m name = 1 + traceToCOM m (m M.! name)

traceToSAN :: M.Map String [String] -> Int
traceToSAN orbits = bfs S.empty [("YOU", 0)]
    where
        bfs :: S.Set String -> [(String, Int)] -> Int
        bfs _ (("SAN", depth):_) = depth - 2
        bfs s ((name, depth):xs)
            | name `S.member` s || isNothing res = bfs s xs
            | otherwise = bfs (name `S.insert` s) (xs ++ adjacent)
            where res = name `M.lookup` orbits
                  adjacent = map (\n -> (n, depth + 1)) $ fromJust res

parsePath :: String -> [(String, String)]
parsePath = map ((\[a, b] -> (b, a)) . splitOn ")") . lines 

parseOrbits :: [(String, String)] -> M.Map String [String] -> M.Map String [String]
parseOrbits [] m = m
parseOrbits ((l, r):xs) m = parseOrbits xs $ M.insertWith (++) l [r] m

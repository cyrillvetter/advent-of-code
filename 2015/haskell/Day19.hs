import Data.List (intercalate, concatMap)
import Data.List.Split (splitOn)
import qualified Data.Set as S

main = do
    (molecule, repls) <- parse <$> readFile "inputs/19.txt"
    print $ S.size $ S.fromList $ concatMap (\(from, to) -> scanReplace from to molecule) repls

scanReplace :: String -> String -> String -> [String]
scanReplace needle repl haystack = fill 1 needle repl $ splitOn needle haystack
    where
        fill :: Int -> String -> String -> [String] -> [String]
        fill split needle repl rest
            | split == length rest = []
            | otherwise = built : fill (split + 1) needle repl rest
            where (left, right) = splitAt split rest
                  built = intercalate needle left ++ repl ++ intercalate needle right

parse :: String -> (String, [(String, String)])
parse s = (molecule, replacements)
    where [r, molecule] = splitOn "\n\n" s
          replacements = map ((\[l, _, r] -> (l, r)) . words) $ lines r


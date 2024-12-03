import Data.List.Split (splitOn)
import Text.Regex.TDFA

mulExp = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

disabler = "don't()"
enabler = "do()"

main = do
    input <- readFile "inputs/3.txt"
    print $ mul input
    print $ mul $ omitDisabled input

mul :: String -> Int
mul = sum . map (\[_, x, y] -> read x * read y) . (=~ mulExp)

omitDisabled :: String -> String
omitDisabled = concatMap (concat . drop 1 . splitOn enabler) . splitOn disabler . (enabler ++)

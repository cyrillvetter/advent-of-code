import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

main = do
    input <- lines <$> readFile "inputs/8.txt"
    let (highestEver, highest) = getLargestRegisterExtremes input M.empty 0
    print highest
    print highestEver

getLargestRegisterExtremes :: [String] -> M.Map String Int -> Int -> (Int, Int)
getLargestRegisterExtremes [] registers highest = (highest, maximum (M.elems registers))
getLargestRegisterExtremes (x:xs) registers highest
    | isSatisfied = getLargestRegisterExtremes xs (M.insert a adjustedValue registers) (max adjustedValue highest)
    | otherwise = getLargestRegisterExtremes xs registers highest
    where [a, o, n, _, b, e, v] = words x
          checkRegister = fromMaybe 0 (b `M.lookup` registers)
          isSatisfied = isConditionSatisfied checkRegister e (read v)
          setRegister = fromMaybe 0 (a `M.lookup` registers)
          adjustedValue
              | o == "inc" = setRegister + read n
              | otherwise = setRegister - read n

isConditionSatisfied :: Int -> String -> Int -> Bool
isConditionSatisfied r o v
    | o == ">" = r > v
    | o == ">=" = r >= v
    | o == "<" = r < v
    | o == "<=" = r <= v
    | o == "==" = r == v
    | o == "!=" = r /= v
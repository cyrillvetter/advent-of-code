import qualified Data.Map.Strict as M

type Memo = M.Map (String, Int) Int

main = do
    input <- words <$> readFile "inputs/11.txt"
    print $ fst $ blink M.empty 25 input
    print $ fst $ blink M.empty 75 input

blink :: Memo -> Int -> [String] -> (Int, Memo)
blink memo times = foldl (\(res, resMem) s -> let (r, m) = run resMem s times in (res + r, m)) (0, memo)
    where
        run :: Memo -> String -> Int -> (Int, Memo)
        run mem r 0 = (1, mem)
        run mem stone i = case mem M.!? (stone, i) of
            Nothing -> (rs, M.insert (stone, i) rs rm)
            Just v  -> (v, mem)
            where (rs, rm) = blink mem (i - 1) (processStone stone)

processStone :: String -> [String]
processStone "0" = ["1"]
processStone stone
    | even (length stone) = [dropLeadingZeroes leftHalf, dropLeadingZeroes rightHalf]
    | otherwise = [show (read stone * 2024)]
    where (leftHalf, rightHalf) = splitAt (length stone `div` 2) stone
          dropLeadingZeroes = max "0" . dropWhile (== '0')

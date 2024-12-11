import qualified Data.Map.Strict as M

type Memo = M.Map (Int, Int) Int

main = do
    input <- map read . words <$> readFile "inputs/11.txt"
    print $ fst $ blink M.empty 25 input
    print $ fst $ blink M.empty 75 input

blink :: Memo -> Int -> [Int] -> (Int, Memo)
blink memo times = foldl
    (\(res, resMem) s ->
        let (r, m) = run resMem s times
        in (res + r, m))
    (0, memo)
    where
        run :: Memo -> Int -> Int -> (Int, Memo)
        run mem _ 0 = (1, mem)
        run mem stone i
            | Just v <- mem M.!? (stone, i)
            = (v, mem)
        run mem stone i = (rs, M.insert (stone, i) rs rm)
            where (rs, rm) = blink mem (i - 1) (processStone stone)

processStone :: Int -> [Int]
processStone 0 = [1]
processStone s
    | even (length str) = [read leftHalf, read rightHalf]
    | otherwise = [s * 2024]
    where str = show s
          (leftHalf, rightHalf) = splitAt (length str `div` 2) str

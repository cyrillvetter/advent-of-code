import Data.Sequence (Seq, fromList, update, index)

main = do
    seq <- fromList . map (read @Int) . lines <$> readFile "inputs/5.txt"
    print $ jump seq 0 0
    print $ jump' seq 0 0

jump :: Seq Int -> Int -> Int -> Int
jump s pos steps
    | pos < 0 || pos >= length s = steps
    | otherwise = jump (update pos (instr + 1) s) (pos + instr) (steps + 1)
    where instr = s `index` pos

jump' :: Seq Int -> Int -> Int -> Int
jump' s pos steps
    | pos < 0 || pos >= length s = steps
    | otherwise = jump' (update pos mut s) (pos + instr) (steps + 1)
    where instr = s `index` pos
          mut = if instr >= 3 then instr - 1 else instr + 1
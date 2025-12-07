import Common (buildCharArray, count)
import Data.List (nub)

import qualified Data.Array.Unboxed as A
import qualified Data.Map as M

type Coord = (Int, Int)

main = do
    grid <- buildCharArray <$> readFile "inputs/07.txt"
    let (((_, start), _):_) = filter ((== 'S') . snd) $ A.assocs grid
        (_, (yBound, _)) = A.bounds grid
    print $ p1 grid yBound 0 [start]
    print $ fst $ p2 grid M.empty yBound (0, start)

p1 :: A.UArray Coord Char -> Int -> Int -> [Int] -> Int
p1 grid yBound y xs
    | y >= yBound = 0
    | otherwise = splitAmount + p1 grid yBound (y + 1) (nub (concat splits))
    where splits = map (\x -> split grid (y, x)) xs
          splitAmount = count ((== 2) . length) splits

p2 :: A.UArray Coord Char -> M.Map Coord Int -> Int -> Coord -> (Int, M.Map Coord Int)
p2 grid mem yBound coord@(y, x)
    | y >= yBound = (1, mem)
    | otherwise = case coord `M.lookup` mem of
        Nothing -> (count, M.insert coord count innerMem)
        Just n  -> (n, mem)
    where splits = split grid coord
          (count, innerMem) =
            foldl (\(c, m) x ->
              let (newC, newM) = p2 grid m yBound (y + 1, x)
              in (c + newC, newM))
            (0, mem) splits

split :: A.UArray Coord Char -> Coord -> [Int]
split grid c@(_, x) = case grid A.! c of
    '^' -> [x - 1, x + 1]
    _   -> [x]

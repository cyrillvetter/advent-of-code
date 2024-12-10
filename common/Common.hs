module Common where

import Data.Char (digitToInt)
import qualified Data.Set as S
import qualified Data.Array.Unboxed as A

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

toTuple3 :: [a] -> (a, a, a)
toTuple3 [x, y, z] = (x, y, z)

addTuples :: Num a => (a, a) -> (a, a) -> (a, a)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subTuples :: Num a => (a, a) -> (a, a) -> (a, a)
subTuples (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

adj4 :: Num a => (a, a) -> [(a, a)]
adj4 c = map (addTuples c) [(1, 0), (-1, 0), (0, 1), (0, -1)]

buildCharArray :: String -> A.UArray (Int, Int) Char
buildCharArray input = A.listArray ((0, 0), (length l - 1, length (head l) - 1)) (concat l)
    where l = lines input

buildIntArray :: String -> A.UArray (Int, Int) Int
buildIntArray input = A.listArray ((0, 0), (length l - 1, length (head l) - 1)) (concat l)
    where l = map (map digitToInt) (lines input)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique list = length list == S.size (S.fromList list)

windowsOf :: Int -> [a] -> [[a]]
windowsOf size list
    | length list < size = []
    | otherwise = take size list : windowsOf size (drop 1 list)

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = check S.empty
    where
        check :: Ord a => S.Set a -> [a] -> a
        check set (x:xs)
            | x `S.member` set = x
            | otherwise = check (x `S.insert` set) xs

module Common where

import qualified Data.Set as S
import qualified Data.IntSet as IS

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

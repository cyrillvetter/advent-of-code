import Data.Array.Unboxed (UArray, assocs, accumArray)

input = 36000000

main = do
    print $ findHouseWithPresents part1Presents
    print $ findHouseWithPresents part2Presents

findHouseWithPresents :: UArray Int Int -> Int
findHouseWithPresents = fst . head . dropWhile ((<= input) . snd) . assocs

part1Presents :: UArray Int Int
part1Presents = accumArray (+) 0 (1, limit) [(house, elf * 10) | elf <- [1..limit], house <- [elf,2*elf..limit]]
    where limit = input `div` 10

part2Presents :: UArray Int Int
part2Presents = accumArray (+) 0 (1, limit) [(house, elf * 11) | elf <- [1..limit], house <- take 50 [elf,2*elf..limit]]
    where limit = input `div` 10

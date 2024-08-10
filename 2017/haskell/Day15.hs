import Data.Bits ((.&.))

aStart = 873
bStart = 583

aFactor = 16807
bFactor = 48271

divider = 2147483647

main = do
    print part1
    print part2

part1 :: Int
part1 =
    let a = iterate (`generateNumber` aFactor) aStart
        b = iterate (`generateNumber` bFactor) bStart
    in length $ filter (uncurry matchLowest16Bits) $ take 40_000_000 $ zip a b

part2 :: Int
part2 =
    let a = filter (\v -> v `mod` 4 == 0) $ iterate (`generateNumber` aFactor) aStart
        b = filter (\v -> v `mod` 8 == 0) $ iterate (`generateNumber` bFactor) bStart
    in length $ filter (uncurry matchLowest16Bits) $ take 5_000_000 $ zip a b

generateNumber :: Int -> Int -> Int
generateNumber x factor = (x * factor) `mod` divider

matchLowest16Bits :: Int -> Int -> Bool
matchLowest16Bits x y = (x .&. 0xFFFF) == (y .&. 0xFFFF)

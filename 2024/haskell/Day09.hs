import Data.Char (digitToInt)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Sequence as S

main = do
    input <- map digitToInt <$> readFile "inputs/9.txt"
    print $ part1 input
    print $ part2 input

part1 :: [Int] -> Int
part1 = checksum . moveFileBlocks . S.fromList . buildDiskBlocks

moveFileBlocks :: S.Seq (Maybe Int) -> [Int]
moveFileBlocks S.Empty = []
moveFileBlocks (Just i S.:<| right) = i : moveFileBlocks right
moveFileBlocks (Nothing S.:<| right) = case right of
    S.Empty -> []
    rest S.:|> Nothing -> moveFileBlocks (Nothing S.<| rest)
    rest S.:|> Just i -> i : moveFileBlocks rest

buildDiskBlocks :: [Int] -> [Maybe Int]
buildDiskBlocks = concat . zipWith (\i size ->
    if even i
    then replicate size (Just (i `div` 2))
    else replicate size Nothing) [0..]

part2 :: [Int] -> Int
part2 input = checksum $ map (fromMaybe 0) $ concatMap (uncurry replicate) defragmented
    where diskSpaces = buildDiskSpaces input
          reverseFiles = reverse $ map (\(size, Just v) -> (size, v)) $ filter (isJust . snd) diskSpaces
          defragmented = foldl moveFile diskSpaces reverseFiles

-- TODO: Use Sequence for part two.
moveFile :: [(Int, Maybe Int)] -> (Int, Int) -> [(Int, Maybe Int)]
moveFile [] _ = []
moveFile (curr@(size, Just v):xs) file
    | (size, v) == file = curr : xs
    | otherwise = curr : moveFile xs file
moveFile ((empty, Nothing):xs) (size, id)
    | size <= empty =
        (size, Just id)
        : (empty - size, Nothing)
        : map (\(i, k) -> if k == Just id then (i, Nothing) else (i, k)) xs
    | otherwise = (empty, Nothing) : moveFile xs (size, id)

buildDiskSpaces :: [Int] -> [(Int, Maybe Int)]
buildDiskSpaces = zipWith (\i size ->
    if even i
    then (size, Just (i `div` 2))
    else (size, Nothing)) [0..]

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

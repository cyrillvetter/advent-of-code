import Data.List.Split (splitOn)
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Sequence as Seq

main = do
    input <- lines <$> readFile "inputs/12.txt"
    let coms = parse input IM.empty
    print $ S.size $ getCommunication (Seq.singleton 0) coms (S.singleton 0)
    print $ countGroups (IM.keys coms) coms

parse :: [String] -> IM.IntMap (Seq.Seq Int) -> IM.IntMap (Seq.Seq Int)
parse [] m = m
parse (x:xs) m = parse xs (IM.insert from (Seq.fromList to) m)
    where [left, right] = splitOn " <-> " x
          from = read left
          to = map read $ splitOn ", " right

getCommunication :: Seq.Seq Int -> IM.IntMap (Seq.Seq Int) -> S.Set Int -> S.Set Int
getCommunication Seq.Empty _ v = v
getCommunication (v Seq.:<| rest) coms visited = getCommunication (rest <> notVisited) coms (v `S.insert` visited)
    where next = coms IM.! v
          notVisited = Seq.filter (`S.notMember` visited) next

countGroups :: [Int] -> IM.IntMap (Seq.Seq Int) -> Int
countGroups [] _ = 0
countGroups (x:xs) m = 1 + countGroups remainingComs m
    where coms = getCommunication (Seq.singleton x) m (S.singleton x)
          remainingComs = filter (`S.notMember` coms) xs
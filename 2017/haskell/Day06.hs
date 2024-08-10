import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)

main = do
    input <- Seq.fromList . map (read @Int) . splitOn "\t" <$> readFile "inputs/6.txt"
    let (loop, depth) = processBlocks input $ S.singleton input
    print depth
    print $ depth - getFirstOccurrenceOf loop input

processBlocks :: Seq.Seq Int -> S.Set (Seq.Seq Int) -> (Seq.Seq Int, Int)
processBlocks s mem
    | redistributed `S.member` mem = (redistributed, S.size mem)
    | otherwise = processBlocks redistributed (redistributed `S.insert` mem)
    where redistributed = redistributeBlocks s

getFirstOccurrenceOf :: Seq.Seq Int -> Seq.Seq Int -> Int
getFirstOccurrenceOf occ s
    | redistributed == occ = 1
    | otherwise = 1 + getFirstOccurrenceOf occ redistributed
    where redistributed = redistributeBlocks s

redistributeBlocks :: Seq.Seq Int -> Seq.Seq Int
redistributeBlocks s = redistribute (maxIndex + 1) amount max (Seq.update maxIndex 0 s)
    where max = maximum s
          maxIndex = fromJust $ max `Seq.elemIndexL` s
          amount = ceiling (fromIntegral max / fromIntegral (Seq.length s))
          redistribute :: Int -> Int -> Int -> Seq.Seq Int -> Seq.Seq Int
          redistribute curr amount left se
              | left < amount = Seq.adjust (+ left) curr se
              | otherwise = redistribute next amount nextAmount (Seq.adjust (+ amount) curr se)
              where next = if curr == length se then 0 else curr + 1
                    nextAmount = if curr == length se then left else left - amount
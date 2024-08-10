import Crypto.Hash
import Data.List (isPrefixOf)
import Data.ByteString.Char8 (pack, fromStrict)

main = do
    input <- readFile "inputs/4.txt"
    let starts5 = findHash input 5 1
    print starts5
    print $ findHash input 6 starts5

findHash :: String -> Int -> Int -> Int
findHash base amt start = head $ dropWhile (not . (zeroes `isPrefixOf`) . md5Hash base) [start..]
    where zeroes = replicate amt '0'

md5Hash :: String -> Int -> String
md5Hash s num = show (hashlazy $ fromStrict $ pack (s ++ show num) :: Digest MD5)

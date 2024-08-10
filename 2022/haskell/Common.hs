module Common where

windowsOf :: Int -> [a] -> [[a]]
windowsOf size list
    | length list < size = []
    | otherwise = take size list : windowsOf size (drop 1 list)

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs
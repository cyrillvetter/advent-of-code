import Common (windowsOf, allUnique)

main = do
    input <- readFile "inputs/6.txt"
    print $ messageStart 4 input
    print $ messageStart 14 input

messageStart :: Int -> String -> Int
messageStart size message = size + (length . takeWhile (not . allUnique) $ windowsOf size message)
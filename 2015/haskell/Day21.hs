data Stats = S Int Int Int deriving (Show)
data Item = I Int Int Int deriving (Show)

playerHp = 100

weapons =
    [ I 8 4 0
    , I 10 5 0
    , I 25 6 0
    , I 40 7 0
    , I 74 8 0 ]

armor =
    [ I 0 0 0
    , I 13 0 1
    , I 31 0 2
    , I 53 0 3
    , I 75 0 4
    , I 102 0 5 ]

rings =
    [ I 0 0 0
    , I 25 1 0
    , I 50 2 0
    , I 100 3 0
    , I 20 0 1
    , I 40 0 2
    , I 80 0 3 ]

main = do
    [hp, dmg, armor] <- map (read . last . words) . lines <$> readFile "inputs/21.txt"
    let bossStats = S hp dmg armor
        fights = map (`fight` bossStats) generateItemCombinations

    print $ minimum $ map fst $ filter snd fights
    print $ maximum $ map fst $ filter (not . snd) fights

fight :: Item -> Stats -> (Int, Bool)
fight (I cost playerDamage playerArmor) (S bossHp bossDamage bossArmor) = (cost, playerAttacks <= bossAttacks)
    where playerAttacks = bossHp `ceilDiv` max 1 (playerDamage - bossArmor)
          bossAttacks = playerHp `ceilDiv` max 1 (bossDamage - playerArmor)

generateItemCombinations :: [Item]
generateItemCombinations = [ mergeItems [w, a, r1, r2] | w <- weapons, a <- armor, [r1, r2] <- combinations 2 rings]

mergeItems :: [Item] -> Item
mergeItems = foldl1 (\(I c1 d1 a1) (I c2 d2 a2) -> I (c1 + c2) (d1 + d2) (a1 + a2))

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n - 1) xs) ++ combinations n xs

ceilDiv :: Int -> Int -> Int
ceilDiv x y = ceiling (fromIntegral x / fromIntegral y)

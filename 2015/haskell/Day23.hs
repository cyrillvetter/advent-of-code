main = do
    input <- lines <$> readFile "inputs/23.txt"
    print $ execInstr input 0 (0, 0)
    print $ execInstr input 0 (1, 0)

execInstr :: [String] -> Int -> (Int, Int) -> Int
execInstr instrs pos r
    | pos < 0 || pos >= length instrs = snd r
    | instrName == "hlf" = execInstr instrs (pos + 1) $ updateReg regName (`div` 2) r
    | instrName == "tpl" = execInstr instrs (pos + 1) $ updateReg regName (*3) r
    | instrName == "inc" = execInstr instrs (pos + 1) $ updateReg regName (+1) r
    | instrName == "jmp" = execInstr instrs (pos + parseNum second) r
    | instrName == "jie" = execInstr instrs (condJump regName even r offset pos) r
    | instrName == "jio" = execInstr instrs (condJump regName (==1) r offset pos) r
    where instr = words $ instrs !! pos
          instrName = head instr
          second = instr !! 1
          regName = head second
          offset = parseNum $ instr !! 2

condJump :: Char -> (Int -> Bool) -> (Int, Int) -> Int -> Int -> Int
condJump 'a' check (a, b) jumpLen pos = if check a then pos + jumpLen else pos + 1
condJump 'b' check (a, b) jumpLen pos = if check b then pos + jumpLen else pos + 1

updateReg :: Char -> (Int -> Int) -> (Int, Int) -> (Int, Int)
updateReg 'a' action (a, b) = (action a, b)
updateReg 'b' action (a, b) = (a, action b)

parseNum :: String -> Int
parseNum ('+':n) = read n
parseNum n = read n

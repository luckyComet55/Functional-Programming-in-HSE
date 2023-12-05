doNTurns :: Int -> Board -> [Board]
doNTurns 0 ini = [ini]
doNTurns n ini = do
        x <- next ini
        doNTurns (n - 1) x
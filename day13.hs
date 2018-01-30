data Dir = Up | Down

data Layer = Layer
    { depth :: Int
    , pos :: Int
    , dir :: Dir
    , range :: Int
    }

instance Show Layer where
    show l = "{R: " ++ show (range l) ++ " P: " ++ show (pos l) ++ " D: " ++ show (depth l) ++ "}\n"

initL (r,d) = Layer 
    { depth=d
    , pos=0
    , dir=Down
    , range=r
    }

atTop    l = (pos l) == 0
atBottom l = (pos l) == (depth l - 1)

tickL :: Layer -> Layer
tickL l = case dir l of
    Down -> 
        if atBottom l
        then l { pos=(pos l - 1), dir=Up }
        else l { pos=(pos l + 1) }
    Up -> 
        if atTop l
        then l { pos=(pos l + 1), dir=Down }
        else l { pos=(pos l - 1) }

severityAt :: Layer -> Int
severityAt layer =
    if (pos layer) == 0
    then (depth layer) * (range layer)
    else 0

initFW :: [(Int, Int)] -> [Layer]
initFW = map initL

fwOverTime :: [Layer] -> [Layer]
-- Applies `tick` to each layer `r` times, where `r` is the layer's range.
fwOverTime = map (\layer -> (iterate tickL layer) !! (range layer))

totalSeverity :: [Layer] -> Int
totalSeverity = (foldr1 (+)) . (map severityAt)
        
main = do
    print $ 24 == (totalSeverity $ fwOverTime $ initFW [(0,3),(1,2),(4,4),(6,4)])
    let soln1 = totalSeverity $ fwOverTime $ initFW inputData
    putStrLn $ "Soln 1: " ++ show soln1

inputData :: [(Int, Int)]
inputData = [
    (0, 4),
    (1, 2),
    (2, 3),
    (4, 4),
    (6, 8),
    (8, 5),
    (10, 8),
    (12, 6),
    (14, 6),
    (16, 8),
    (18, 6),
    (20, 6),
    (22, 12),
    (24, 12),
    (26, 10),
    (28, 8),
    (30, 12),
    (32, 8),
    (34, 12),
    (36, 9),
    (38, 12),
    (40, 8),
    (42, 12),
    (44, 17),
    (46, 14),
    (48, 12),
    (50, 10),
    (52, 20),
    (54, 12),
    (56, 14),
    (58, 14),
    (60, 14),
    (62, 12),
    (64, 14),
    (66, 14),
    (68, 14),
    (70, 14),
    (72, 12),
    (74, 14),
    (76, 14),
    (80, 14),
    (84, 18),
    (88, 14)]

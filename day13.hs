import Data.List

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

tick :: Layer -> Layer
tick l = case dir l of
    Down ->
        if atBottom
        then l { pos=pos l - 1, dir=Up }
        else l { pos=pos l + 1 }
    Up ->
        if atTop
        then l { pos=pos l + 1, dir=Down }
        else l { pos=pos l - 1 }
    where
        atTop    = pos l == 0
        atBottom = pos l == (depth l - 1)

tickN :: Int -> Layer -> Layer
-- Just like apply `tick` n times, but optimized.
tickN n l = (iterate tick l) !! (n `rem` d)
    where
        -- Guards repeat their steps every (2*depth - 2) ticks,
        d = 2 * depth l - 2

severityAt :: Layer -> Int
severityAt layer =
    if pos layer == 0
    then depth layer * range layer
    else 0

initFW :: [(Int, Int)] -> [Layer]
initFW = map initL

fwOverTime :: Int -> [Layer] -> [Layer]
-- Applies `tick` to each layer `r` times
-- where r is the layer's range, plus t
fwOverTime offset = map (\l -> tickN (range l + offset) l)

totalSeverity :: [Layer] -> Int
totalSeverity = sum . map severityAt

soln1 = totalSeverity . fwOverTime 0
soln2 fw = fmap fst . find notCaught . map (\t -> (t, fwOverTime t fw)) $ [0..]

notCaught :: (Int, [Layer]) -> Bool
notCaught (t, fw) = all (\l -> pos l /= 0) fw

main = do
    unitTests
    let fw = initFW inputData
    putStrLn $ "Soln 1: " ++ show (soln1 fw)
    putStrLn $ "Soln 2: " ++ show (soln2 fw)

unitTests = do
    let testFW = initFW [(0,3),(1,2),(4,4),(6,4)]
    print $ 24 == soln1 testFW
    print $ Just 10 == soln2 testFW

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

import Text.Printf

type Predicate = (Int -> Bool)

gen :: Int -> Int -> [Int]
gen factor seed = iterate f (f seed)
    where
        f n = (n * factor) `rem` 2147483647

to_binary :: [Int] -> [String]
to_binary nums = map (toLength16 . printf "%b") nums

toLength16 :: String -> String
toLength16 s =
    if (length s) > 16
    then drop ((length s) - 16) s
    else (replicate (16 - length s) '0') ++ s

judgeSeqs :: [String] -> [String] -> Int
judgeSeqs as bs = length . filter (\(a,b) -> a == b) $ zip as bs

judge :: Predicate -> Predicate -> Int -> Int
judge predA predB numSamples = judgeSeqs as bs
    where
        as = outputs predA $ gen factor_a seed_a
        bs = outputs predB $ gen factor_b seed_b
        outputs filt = take numSamples . to_binary . filter filt


divBy :: Int -> Int -> Bool
divBy n = (==0) . (`rem` n)

seed_a = 591
seed_b = 393
factor_a = 16807
factor_b = 48271

main = do
    let soln1 = judge (const True) (const True) $ 40 * (10^6)
    let soln2 = judge (divBy 4)    (divBy 8)    $  5 * (10^6)
    putStrLn $ "Soln 1: " ++ (show soln1)
    putStrLn $ "Soln 2: " ++ (show soln2)
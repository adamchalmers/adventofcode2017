import Text.Printf

gen :: Int -> Int -> [Int]
gen factor seed = iterate f (f seed)
	where
		f n = (n * factor) `rem` 2147483647

gen_a = gen 16807
gen_b = gen 48271

to_binary :: [Int] -> [String]
to_binary nums = map (adjust . printf "%b") nums

adjust :: String -> String
adjust s =
	if (length s) > 16
	then drop ((length s) - 16) s
	else (replicate (16 - length s) '0') ++ s

outputs filt n gen start = take n $ to_binary $ filter filt $ gen start

judge :: [String] -> [String] -> Int
judge as bs = length . filter (\(a,b) -> a == b) $ zip as bs

solve f g start_a start_b n = judge as bs
	where
		as = outputs f n gen_a start_a
		bs = outputs g n gen_b start_b

divBy :: Int -> Int -> Bool
divBy n = (==0) . (`rem` n)

main = do
	let start_a = 591
	let start_b = 393
	let soln1 = solve (const True) (const True) start_a start_b $ 40 * (10^6)
	let soln2 = solve (divBy 4)    (divBy 8)    start_a start_b $  5 * (10^6)
	putStrLn $ "Soln 1: " ++ (show soln1)
	putStrLn $ "Soln 2: " ++ (show soln2)
import Data.Char
import Data.Bits
import Prelude hiding (group)
import Numeric (showHex, showIntAtBase)

type NumRange = [Int]

multiHash :: NumRange -> [Int] -> NumRange
multiHash nums lens =
    multiHash' 0 0 nums lens 16

group :: Int -> [Int] -> [[Int]]
group n = foldr f [] where
    f x [] = [[x]]
    f x output@(o:os) =
        if (length o) == n
        then ([x]):output
        else (x:o):os

showHex' i = twoLong (showHex i "")
twoLong [x] = "0" ++ [x]
twoLong xs = xs

showDense :: [Int] -> String
showDense xs = concat $ map showHex' xs

sparseToDense :: NumRange -> [Int]
sparseToDense sparse = map (foldr1 xor) (group 16 sparse)

multiHash' :: Int -> Int -> NumRange -> [Int] -> Int -> NumRange
multiHash' curr skip nums lens 0 = nums
multiHash' curr skip nums lens n = 
    multiHash' curr' skip' nums' lens (n-1)
    where
        (nums', curr', skip') = twist curr skip nums lens

hash curr skip nums lens = p * q
    where
        (p:q:_) = fst $ twist curr skip nums lens
        fst (x,_,_) = x

twist :: Int -> Int -> NumRange -> [Int] -> (NumRange, Int, Int)
twist curr skip nums lens = foldl (twistAcc) (nums, curr, skip) lens

twistAcc :: (NumRange, Int, Int) -> Int -> (NumRange, Int, Int)
twistAcc (nums, curr, skip) len = (nums', curr', skip') where
    n     = length nums
    nums' = reverseFrom nums curr len
    curr' = (curr + len + skip) `rem` n
    skip' = (skip + 1)          `rem` n

-- Reverse the list from element #i to #(i + len), working circularly.
reverseFrom :: NumRange -> Int -> Int -> [Int]
reverseFrom nums i len = 
    (rotR i) . (reverseFirst len) . (rotL i) $ nums
    where
        reverseFirst n list = (reverse $ take n list) ++ drop n list
        rotL i list         = (drop i list) ++ (take i list)
        rotR i list         = rotL ((length list) - i) list

main = do
    unitTests
    let input = [225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110]
    let soln1 = hash 0 0 [0..255] input
    putStrLn $ "Soln1: " ++ (show soln1)
    --soln2
    let inputAscii = (map ord "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110") ++ [17,31,83,47,23]
    let soln2 = showDense $ sparseToDense $ multiHash [0..255] inputAscii
    putStrLn $ "Soln2: " ++ (soln2)

unitTests = do
    print $ (reverseFrom [0..4]      0 3) == [2,1,0,3,4]
    print $ (reverseFrom [2,1,0,3,4] 3 4) == [4,3,0,1,2]
    print $ (reverseFrom [4,3,0,1,2] 3 1) == [4,3,0,1,2]
    print $ (reverseFrom [4,3,0,1,2] 1 5) == [3,4,2,1,0]

    print $ (twistAcc ([0..4],      0, 0) 3) == ([2,1,0,3,4], 3, 1)
    print $ (twistAcc ([2,1,0,3,4], 3, 1) 4) == ([4,3,0,1,2], 3, 2)
    print $ (twistAcc ([4,3,0,1,2], 3, 2) 1) == ([4,3,0,1,2], 1, 3)
    print $ (twistAcc ([4,3,0,1,2], 1, 3) 5) == ([3,4,2,1,0], 4, 4)

    print $ (twist 0 0 [0..4] [3,4,1,5]) == ([3,4,2,1,0], 4, 4)
    print $ (hash 0 0 [0..4] [3,4,1,5]) == 12

    print $ group 16 [0..15] == [[0..15]]
    print $ group 16 [0..31] == [[0..15], [16..31]]

    let someInts = [65,27,9,1,4,3,40,50,91,7,6,0,2,5,68,22] :: [Int]
    print $ (foldr1 xor someInts) == 64


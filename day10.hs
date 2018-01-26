import Data.List

hash nums lens = p * q
    where
        (p:q:_) = fst $ twist nums lens
        fst (x,_,_) = x

twist nums lens = foldl (twistAcc) (nums, 0, 0) lens

twistAcc :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
twistAcc (nums, curr, skip) len = (nums', curr', skip') where
    n     = length nums
    nums' = reverseFrom nums curr len
    curr' = (curr + len + skip) `rem` n
    skip' = (skip + 1)          `rem` n

-- Reverse the list from element #i to #(i + len), working circularly.
-- 1. Rotate the list so that #i -> #0
-- 2. Rotate from 0 to len
-- 3. Rotate the list #0 -> #i
reverseFrom :: [Int] -> Int -> Int -> [Int]
reverseFrom nums i len = 
    (rotR i) . (reverseFirst len) . (rotL i) $ nums
    where
        reverseFirst n list = (reverse $ take n list) ++ drop n list
        rotL i list         = (drop i list) ++ (take i list)
        rotR i list         = rotL ((length list) - i) list

main = do
    unitTests
    let input = [225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110]
    print $ hash [0..255] input

unitTests = do
    print $ (reverseFrom [0..4]      0 3) == [2,1,0,3,4]
    print $ (reverseFrom [2,1,0,3,4] 3 4) == [4,3,0,1,2]
    print $ (reverseFrom [4,3,0,1,2] 3 1) == [4,3,0,1,2]
    print $ (reverseFrom [4,3,0,1,2] 1 5) == [3,4,2,1,0]

    print $ (twistAcc ([0..4],      0, 0) 3) == ([2,1,0,3,4], 3, 1)
    print $ (twistAcc ([2,1,0,3,4], 3, 1) 4) == ([4,3,0,1,2], 3, 2)
    print $ (twistAcc ([4,3,0,1,2], 3, 2) 1) == ([4,3,0,1,2], 1, 3)
    print $ (twistAcc ([4,3,0,1,2], 1, 3) 5) == ([3,4,2,1,0], 4, 4)

    print $ (twist [0..4] [3,4,1,5]) == ([3,4,2,1,0], 4, 4)
    print $ (hash [0..4] [3,4,1,5]) == 12

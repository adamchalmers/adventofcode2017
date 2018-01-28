{-# LANGUAGE DeriveFunctor #-}

import Data.Char (ord)
import Data.Bits (xor)
import Numeric (showHex)
import Data.List.Split (splitOn)

allBytes = Sparse [0..255]
numBytes = 256

newtype Sparse a = Sparse a deriving (Eq, Functor)
type SparseHash = Sparse [Int]

partition :: Int -> [Int] -> [[Int]]
-- Partition input list into sublists of length k
partition k = foldr f [] where
    f x [] = [[x]]
    f x output@(o:os) =
        if (length o) == k
        then ([x]):output
        else (x:o):os

showDense :: [Int] -> String
-- Format a dense hash as a hex string
showDense xs = concat $ map (\i -> twoLong (showHex i "")) xs where
    twoLong [x] = "0" ++ [x]
    twoLong xs = xs

sparseToDense :: SparseHash -> [Int]
sparseToDense (Sparse nums) = map (foldr1 xor) (partition 16 nums)

multiHash :: [Int] -> Int -> SparseHash
-- Run multiple rounds of the hashing algorithm
multiHash lens numRounds = let
    mh _ _ sparseHash _ 0 = sparseHash
    mh curr skip sparseHash lens n =
        mh curr' skip' sparseHash' lens (n-1) where
            (sparseHash', curr', skip') = hash lens curr skip sparseHash
    in
    mh 0 0 allBytes (lens ++ [17, 31, 73, 47, 23]) numRounds where

hash :: [Int] -> Int -> Int -> SparseHash -> (SparseHash, Int, Int)
-- Run one round of the hashing algorithm
hash lens curr skip nums = foldl acc (nums, curr, skip) lens where
    acc (nums, curr, skip) len = (nums', curr', skip') where
        nums' = twist curr len nums
        curr' = (curr + len + skip) `rem` numBytes
        skip' = (skip + 1)          `rem` numBytes

twist :: Int -> Int -> SparseHash -> SparseHash
-- The 'pinch-and-twist' permutation described in the problem
twist i len =
    fmap $ (rotR i) . (reverseFirst len) . (rotL i) where
        reverseFirst n list = (reverse $ take n list) ++ drop n list
        rotL i list         = (drop i list) ++ (take i list)
        rotR i list         = rotL ((length list) - i) list

getSoln :: (SparseHash, Int, Int) -> Int
getSoln = foldr1 (*) . take 2 . (\(Sparse xs,_,_) -> xs)

main = do
    let input = "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110"
    let inputInts = map read $ splitOn "," input :: [Int]
    let soln1 = hash inputInts 0 0 allBytes
    putStrLn $ "Soln1: " ++ (show $ getSoln soln1)
    let inputAscii = map ord input
    let soln2 = showDense $ sparseToDense $ multiHash inputAscii 64
    putStrLn $ "Soln2: " ++ (soln2)

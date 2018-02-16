{-# LANGUAGE LambdaCase #-}

import KnotHash
import Control.Monad as M
import Data.Vector as V
import qualified Data.List as L
import Prelude hiding (map, filter, length, (++), concatMap, sum, take)

-----------------------------------------
-- Part 1

data Cell = Free | Used deriving (Eq)

makeGrid s = map (toRow . makeInput) (fromList [0..127]) where
    makeInput i =
        s ++ singleton '-' ++ (fromList $ show i)
    toRow =
        (concatMap (fromList . (L.map f) . hexToBin)) . (fromList . knotHash . toList)
    f = \case
        '0' -> Free
        '1' -> Used
    hexToBin = \case
        '0' -> "0000"
        '1' -> "0001"
        '2' -> "0010"
        '3' -> "0011"
        '4' -> "0100"
        '5' -> "0101"
        '6' -> "0110"
        '7' -> "0111"
        '8' -> "1000"
        '9' -> "1001"
        'a' -> "1010"
        'b' -> "1011"
        'c' -> "1100"
        'd' -> "1101"
        'e' -> "1110"
        'f' -> "1111"


-----------------------------------------
-- Part 2


type Grid a = Vector (Vector a)
type Point = (Int, Int)

get grid (x,y)   = (grid ! x) ! y
set grid (x,y) v = grid // [(x, (grid ! x) // [(y,v)])]

countCC :: Grid Cell -> Int
countCC grid =
    countCC' grid (0,0) 1
        where
            countCC' grid p@(x,y) counter
                | x == (length grid)       = counter - 1
                | get grid p == Free = countCC' grid (step grid p) counter
                | otherwise = countCC' grid' (step grid p) (counter + 1)
                    where
                        grid' = dfs grid p

step :: Grid a -> Point -> Point
step grid (x,y)
    | y == (length grid - 1) = (x+1, 0)
    | otherwise              = (x, y+1)

dfs :: Grid Cell -> Point -> Grid Cell
-- DFS from the current cell in all 4 directions,
-- only visiting Used nodes and marking them as Free once they've been visited.
dfs grid p@(x,y)
    | get grid p == Free = grid
    | otherwise =
        let
            dirs =
                [ (x+1,y)
                , (x-1,y)
                , (x,y+1)
                , (x,y-1)
                ]
            pointFilter (x,y) =
                   x >= 0
                && y >= 0
                && x < length grid
                && y < length grid
            validDirs = L.filter pointFilter dirs
            grid' = set grid p Free
        in
        L.foldl dfs grid' validDirs

main = do
    let example = "flqrgnkx"
    let real = "jxqlasbh"
    let grid = makeGrid $ fromList real
    putStrLn "Soln1"
    print $ sum $ map (length . filter (== Used)) grid
    putStrLn "Soln2"
    print $ countCC grid




{-# LANGUAGE LambdaCase #-}

import KnotHash
import Control.Monad as M
import Data.Vector as V
import qualified Data.List as L
import Prelude hiding (map, filter, length, (++), concatMap, sum, take)

data Cell = Free | Used deriving (Eq)
instance Show Cell where
    show Free = "."
    show Used = "#"

type Grid a = Vector (Vector a)
type Point = (Int, Int)

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

index grid (x,y) = (grid ! x) ! y

countCC :: Grid Cell -> Int
countCC grid = labelGrid grid (0,0) 1

step :: Grid a -> Point -> Point
step grid (x,y)
    | y == (length grid - 1) = (x+1, 0)
    | otherwise              = (x, y+1)

labelGrid :: Grid Cell -> Point -> Int -> Int
labelGrid grid p@(x,y) counter
    | x == (length grid)       = counter
    | index grid p == Free = labelGrid grid (step grid p) counter
    | otherwise = labelGrid grid' (step grid p) (counter + 1)
        where
            found = dfs grid [p] []
            grid' = markFree found grid

markFree :: [Point] -> Grid Cell -> Grid Cell
-- TODO: Mark all points in `found` as Free
markFree found = id

dfs :: Grid Cell -> [Point] -> [Point] -> [Point]
dfs grid (q:qs) found =
    -- TODO: Run a DFS from `p` that only visits neighbours if they're used.
    found

main = do
    let example = "flqrgnkx"
    let real = "jxqlasbh"
    let grid = makeGrid $ fromList example
    putStrLn "Soln1"
    print $ sum $ map (length . filter (== Used)) grid



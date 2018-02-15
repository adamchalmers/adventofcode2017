{-# LANGUAGE LambdaCase #-}

import KnotHash
import Control.Monad as M
import Data.Vector as V
import qualified Data.List as L
import Prelude hiding (map, filter, length, (++), concatMap, sum, take)

data Cell = Free | Used | Group Int deriving (Eq)
instance Show Cell where
    show Free = "."
    show Used = "#"
    show (Group n) = show n

type Counter = Int
type Answer = Int

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

index grid x y = (grid ! x) ! y

labelGrid grid = labelGrid' grid 0 0 1

labelGrid' grid 127 127 v = (grid, v)
labelGrid' grid x y v = let
        (x', y') =
            case (x, y) of
                (i, 127) -> (i+1, 0)
                (i, j)   -> (i, j+1)
        (cell, incV) = label grid x y v
        v' = if incV then (v + 1) else v
        grid' = grid // [(x, row)]
        row = (grid ! x) // [(y, cell)]
    in
    labelGrid' grid' x' y' v'

label :: Vector (Vector Cell) -> Int -> Int -> Counter -> (Cell, Bool)
-- Checks previously-processed cells to determine this cell's value.
-- Returns the cell's value and whether `v` should be incremented.
label grid x y v =
    let
        me   =                   index grid x y
        left = if (x-1 >= 0) then index grid (x-1) y else Free
        up   = if (y-1 >= 0) then index grid x (y-1) else Free
    in
    case me of
        Used ->
            case (up, left) of
                (Group n, _) -> (Group n, False)
                (_, Group n) -> (Group n, False)
                _            -> (Group v, True)
        Free ->
            (me, False)
        _ -> error $ Prelude.concat
            [ "Crashed. "
            , show left
            , ","
            , show up
            , " ("
            , show x
            , ","
            , show y
            , ")"
            ]


main = do
    let example = "flqrgnkx"
    let real = "jxqlasbh"
    let grid = makeGrid $ fromList example
    putStrLn "Soln1"
    print $ sum $ map (length . filter (== Used)) grid
    putStrLn "Soln2"
    let (regions, nRegions) = labelGrid grid
    V.forM_ regions (print)
    print nRegions



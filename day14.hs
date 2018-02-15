{-# LANGUAGE LambdaCase #-}

import KnotHash
import Control.Monad as M

data Cell = Free | Used | Group Int deriving (Eq)

makeGrid :: String -> [[Cell]]
makeGrid s = map (toRow . makeInput) [0..127] where
    makeInput i =
        s ++ "-" ++ show i
    toRow =
        (concatMap ((map f) . hexToBin)) . knotHash
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

main = do
    let grid = makeGrid "jxqlasbh"
    putStrLn "Soln1"
    print $ sum $ map (length . filter (== Used)) grid


{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Char (isAlpha)
import Data.List as L
import Data.Map.Strict as M hiding ((!))
import Data.Maybe
import Data.String.QQ
import Data.Vector as V

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

data Val
    = Reg Register -- Value is read from register
    | Qty Int      -- Value is a literal

type Register = Char

instance Read Val where
    readsPrec _ s =
        if isAlpha (L.head s)
        then [(Reg (L.head s), "")]
        else [(Qty (read s), "")]

data Instruction
    = Snd Val
    | Set Register Val
    | Add Register Val
    | Mul Register Val
    | Mod Register Val
    | Rcv Val
    | Jgz Val Val

instance Read Instruction where
    readsPrec _ s = [(instr, "")]
        where
            instr = case L.head $ words s of
                "snd" -> Snd $ read fst
                "set" -> Set (L.head fst) snd
                "add" -> Add (L.head fst) snd
                "mul" -> Mul (L.head fst) snd
                "mod" -> Mod (L.head fst) snd
                "rcv" -> Rcv $ read fst
                "jgz" -> Jgz (read fst) snd
            fst = (!! 1) . words $ s
            snd = read . (!! 2) . words $ s

-------------------------------------------------------------------------------
-- Simulating
-------------------------------------------------------------------------------

data State = State
    { registers    :: Map Register Int
    , line         :: Int
    , lastSound    :: Int
    , lastRecover  :: Int
    , firstRecover :: Maybe Int
    } deriving (Show)

initState = State
    { registers     = M.fromList []
    , line          = 0
    , lastSound     = 0
    , lastRecover   = 0
    , firstRecover  = Nothing
    }

exec instructions s = s' { line = line s' + 1 }
    where s' = exec' instructions s

exec' :: Vector Instruction -> State -> State
exec' instructions s = case instructions ! line s of
    Snd v ->
        s { lastSound = val v}
    Set reg v ->
        s { registers = M.insert reg (val v) (registers s)}
    Add reg v ->
        s { registers = insertWithDefault 0 (+) reg (val v) (registers s)}
    Mul reg v ->
        s { registers = insertWithDefault 0 (*) reg (val v) (registers s)}
    Mod reg v ->
        s { registers = insertWithDefault 0 (flip rem) reg (val v) (registers s)}
    Rcv v
        | val v == 0                 -> s
        | isNothing $ firstRecover s -> s { lastRecover = lastSound s
                                          , firstRecover = Just $ lastSound s
                                          }
        | otherwise                  -> s { lastRecover = lastSound s }
    Jgz v offset ->
        if val v > 0
        then s { line = val offset + line s - 1 }
        else s
    where
        val (Reg register) = findWithDefault 0 register $ registers s
        val (Qty i) = i

insertWithDefault :: (Ord k) => a -> (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithDefault defaultVal f key val1 someMap =
    M.insert key (f val1 val2) someMap
    where
        val2 = findWithDefault defaultVal key someMap

runSimUntil :: (State -> Bool) -> Vector Instruction -> State -> State
runSimUntil pred instructions s =
    if (currLine >= 0) && (currLine < V.length instructions) && pred s
    then runSimUntil pred instructions (exec instructions s)
    else s
    where
        currLine = line s

main = do
    let instructions = V.fromList . L.map read . lines $ problemInput
    print $ runSimUntil (isNothing . firstRecover) instructions initState

testInput :: String
testInput = [s|set a 1
add a 2
mul a a
mod a 5
snd a
set a b
rcv a
jgz a -1
set a 1
jgz a -2|]

problemInput :: String
problemInput = [s|set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 316
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19|]
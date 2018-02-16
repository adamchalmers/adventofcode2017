{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Char (isAlpha)
import Data.List as L
import Data.Map as M
import Data.String.QQ

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

data Val = Reg Register | Qty Int
type Register = Char

data Instruction
    = Snd Val
    | Set Register Val
    | Add Register Val
    | Mul Register Val
    | Mod Register Val
    | Rcv Val
    | Jgz Val Val

parseValue :: String -> Val
parseValue s
    | isAlpha (head s) = Reg (head s)
    | otherwise        = Qty (read s)

parseInstruction :: String -> Instruction
parseInstruction s = case op of
    "snd" -> Snd $ parseValue fst
    "set" -> Set (head fst) snd
    "add" -> Add (head fst) snd
    "mul" -> Mul (head fst) snd
    "mod" -> Mod (head fst) snd
    "rcv" -> Rcv $ parseValue fst
    "jgz" -> Jgz (parseValue fst) snd
    where
        op = head . words $ s
        fst = (!! 1) . words $ s
        snd = parseValue . (!! 2) . words $ s

-------------------------------------------------------------------------------
-- Simulating
-------------------------------------------------------------------------------

data State = State
    { regs :: Map Register Int
    , line :: Int
    , lastSound :: Int
    , lastRecover :: Int
    }

initState = State
    { regs=M.fromList []
    , line=0
    , lastSound=0
    , lastRecover=0}

exec :: State -> Instruction -> State
exec s = \case
    Snd v ->
        s { lastSound = val v}
    Set reg v ->
        s { regs = M.insert reg (val v) (regs s)}
    Add reg v ->
        s { regs = M.insertWith (+) reg (val v) (regs s)}
    Mul reg v ->
        s { regs = M.insertWith (*) reg (val v) (regs s)}
    Mod reg v ->
        s { regs = M.insertWith rem reg (val v) (regs s)}
    Rcv v ->
        if (val v) /= 0
        then s { lastRecover = (lastSound s) }
        else s
    Jgz v offset ->
        if (val v) > 0
        then s { line = (val offset) + (line s) }
        else s
    where
        val (Reg register) = findWithDefault 0 register (regs s)
        val (Qty i) = i

main = do
    let instructions = L.map parseInstruction . lines $ problemInput
    let after = L.foldl exec initState instructions
    putStrLn $ "Soln 1: " ++ (show $ lastSound after)


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
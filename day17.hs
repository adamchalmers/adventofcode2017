import Control.Monad as M
import Data.Vector.Unboxed as V
import Prelude hiding (take, drop, length, (++))

offset = 344

simulateSpinlock :: (a -> Int -> Int -> a) -> Int -> Int -> Int -> a -> a
simulateSpinlock f last i val answer =
    if last == val
    then answer
    else simulateSpinlock f last i' (val + 1) $ f answer i' val
        where
            i' = ((i + offset) `rem` val) + 1

ans1 vec i' val = (take i' vec) ++ singleton val ++ (drop i' vec)
ans2 afterZero i' val = if i' == 1 then val else afterZero

main = do
    let iters = 2018
    let spinlock = simulateSpinlock ans1 iters 0 1 (singleton 0)
    let i = fmap (\i -> spinlock ! (i + 1)) (elemIndex (iters - 1) spinlock)

    putStrLn "Soln 1"
    print i

    let soln2 = simulateSpinlock ans2 50000000 0 1 1
    putStrLn "Soln 2"
    print soln2


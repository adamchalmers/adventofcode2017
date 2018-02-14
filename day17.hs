import Control.Monad as M
import Data.Vector.Unboxed as V
import Prelude hiding (take, drop, length, (++))

offset = 344

simulate :: (a -> Int -> Int -> a) -> Int -> a -> a
simulate f numIters partialSoln = simulate' 0 1 partialSoln
    where
        simulate' i val partialSoln =
            if val == numIters
            then partialSoln
            else simulate' i' (val + 1) partialSoln'
                where
                    i' = ((i + offset) `rem` val) + 1
                    partialSoln' = f partialSoln i' val

ans1 vec i' val =
    take i' vec ++ singleton val ++ drop i' vec

ans2 afterZero i' val =
    if i' == 1 then val else afterZero

valAfter :: (Unbox a, Eq a) => a -> Vector a -> Maybe a
valAfter val xs = fmap (\i -> xs ! (i + 1)) (elemIndex val xs)

main = do
    let soln1 = simulate ans1 2018 (singleton 0)
    putStrLn "Soln 1"
    print $ valAfter 2017 soln1

    let soln2 = simulate ans2 5000 1
    putStrLn "Soln 2"
    print soln2


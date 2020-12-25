module Common ( parse, result, play ) where

import Data.List ((\\))
import qualified Data.Array.ST as A
import Data.Array.ST (STUArray, writeArray, readArray, runSTUArray, )
import Control.Monad (forM_)
import Data.STRef as STR ( newSTRef, readSTRef, writeSTRef )
import Data.Array.Base ((!), UArray )
import Data.Foldable (for_)


parse :: Int -> String -> [Int]
parse m s = let
    xs = map (read . (:[])) s
    ys = [maximum xs + 1 ..m]
    xs' = xs ++ ys ++ [head xs]
    in xs'

result :: Int -> Int -> UArray Int Int -> [Int]
result i n arr = take n $ drop 1 $ iterate (arr !) i

play :: Int -> [Int] -> UArray Int Int
play n m = runSTUArray $ do
    currRef <- STR.newSTRef $ head m
    arr <- A.newArray_ (1, length m - 1)
    for_ (zip m (tail m)) (uncurry (writeArray arr))

    for_ [1..n] $ \_ -> do
        curr <- readSTRef currRef
        a <- readArray arr curr
        b <- readArray arr a
        c <- readArray arr b
        cFollower <- readArray arr c

        let maxRest = maximum $ maxes \\ [curr,a,b,c]
        let minRest = maximum $ [curr-4..curr-1] \\ [a,b,c]
        let currIsMin = curr == 1 || curr < 5 && all (`elem` [a,b,c]) [1..curr-1]
        let dst = if currIsMin then maxRest else minRest
        dstFollower <- readArray arr dst

        writeArray arr curr cFollower
        writeArray arr dst a
        writeArray arr c dstFollower

        writeSTRef currRef cFollower
    pure arr
    where
        maxes = [length m - 6..length m - 1]
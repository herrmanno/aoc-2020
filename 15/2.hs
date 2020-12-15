import Control.Monad.ST ( runST, ST )
import Data.STRef ( newSTRef, readSTRef, writeSTRef )
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad (forM_)


main = print $ run (input, n) where
    n = 30000000
    input = [11,0,1,10,5,19]
    -- input = [0,3,6]

--     (initial numbers, target index) -> Result
run :: ([Int], Int) -> Int
run (ns,j) = runST $ do
    let n_1 = last ns
    let i = length ns
    m <- V.replicate j (-1,-1)
    forM_ (zip [0..] ns) (\(i,n) -> V.write m n (i,-1))

    lastref <- newSTRef n_1

    forM_ [i..j-1] $ \i -> do
        last <- readSTRef lastref
        occurences <- V.read m last
        let push = V.modify m (\(a,b) -> (i,a))
        let last' = case l occurences of
                    1 -> 0
                    _ -> d occurences
        case last' of
                0 -> push 0
                _ -> push $ d occurences
        writeSTRef lastref last'

    readSTRef lastref

    where   l (a,b) = sum $ map fromEnum [a /= -1, b /= -1]
            d = uncurry (-)
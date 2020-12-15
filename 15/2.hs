import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Control.Monad.State ( evalState, MonadState(put, get), State )
import Data.Maybe ( fromMaybe )

input = [11,0,1,10,5,19]
initMap = IM.fromList $ zip input (map (:[]) [0..])

main = print $ evalState run (initMap, last input, length input, 30000000) -- 30000000)

-- (map from Numbers to its occurences, last spoke number, curr ndex, target index)
type S = (IntMap [Int], Int, Int, Int)

run :: State S Int
run = do
    (m,n_1,i,j) <- get
    let occurences = IM.findWithDefault [] n_1 m
    let push i = Just . (:)i . take 1 . fromMaybe []
    let s' = case length occurences of
                1 -> (IM.alter (push i) 0 m, 0, i+1, j) -- speak 0
                _ -> let d = foldr1 (-) occurences in (IM.alter (push i) d m, d, i+1, j) -- speak diff of occurences[0] occurences[1]
    put s'
    if i >= j then
        return n_1
    else
        run
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Control.Monad.State

type Seats = M.Map (Int,Int) Char

main = readFile "input.txt" <&> lines <&> run >>= print

run :: [String] -> Int
run xs = let infl = [0..] :: [Int]
             zs = concat $ zipWith (\i ls -> zipWith (\j c  -> ((i,j),c)) infl ls) infl xs
             ms = M.fromList zs :: Seats
         in evalState go [ms]

go :: State [Seats] Int
go = do
    state <- get
    let s' = remap $ head state
    put (s':(tail state))
    if s' `elem` state then return (seatCount s') else go

remap :: Seats -> Seats
remap xs = M.mapWithKey f xs where
    f (i,j) v = case v of
        '.' -> '.'
        'L' -> if (seatsAround i j) == 0 then '#' else 'L'
        '#' -> if (seatsAround i j) >= 4 then 'L' else '#'
    seatsAround i j = length $ filter (=='#') $ catMaybes [xs M.!? (i+x,j+y) | x <- [-1,0,1], y <- [-1,0,1], (x,y) /= (0,0) ]

seatCount :: Seats -> Int
seatCount = M.foldr f  0where
    f '#' acc = acc + 1
    f _ acc = acc

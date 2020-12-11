import Data.Functor ((<&>))
import Data.Maybe (catMaybes,listToMaybe)
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
        'L' -> if (seatsAround i j xs) == 0 then '#' else 'L'
        '#' -> if (seatsAround i j xs) >= 5 then 'L' else '#'

seatsAround :: Int -> Int -> Seats -> Int
seatsAround i j xs = length $ filter (=='#') allSeatsAround where
    allSeatsAround = catMaybes [seatsInDirection (x,y) | x <- [-1,0,1], y <- [-1,0,1], (x,y) /= (0,0) ]
    seatsInDirection (x,y) =
        let nextSeats = map (xs M.!?) $ iterate (\(a,b) -> (a+x,b+y)) (i+x,j+y)
            free (Just '.') = True
            free _ = False
        in listToMaybe $catMaybes $ filter (not . free) $ takeWhile (/=Nothing) nextSeats

seatCount :: Seats -> Int
seatCount = M.foldr f  0 where
    f '#' acc = acc + 1
    f _ acc = acc

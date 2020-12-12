import Data.Functor ((<&>))
import Data.Maybe (listToMaybe,fromMaybe)
import Control.Monad.State (evalState, MonadState(put, get), State)

data Command = N | E | S | W | L | R | F deriving (Show, Read, Enum)
type Instr = (Command, Int)
type Point = (Int,Int)
type S = (Point,Point,[Instr]) -- waypoint coords, ship coords, remaining instructions

main = (readFile "input.txt" <&> lines) >>= (print . run)

run :: [String] -> Int
run xs = evalState move ((10,-1),(0,0),xs') where xs' = map parse xs

parse :: String -> Instr
parse s = (read [head s], read $ tail s)


move :: State S Int
move = do
    (w,s,is) <- get
    let (w',s') = move' w s (fromMaybe (F,0) $ listToMaybe is)
    put (w',s',tail is)
    if null is then return (abs (fst s') + abs (snd s')) else move

-- waypoint -> shippoint -> instructions -> (new waypoint, new shippoint)
move' :: Point -> Point -> Instr -> (Point,Point)
move' (x,y) s (N,n) = ((x,y-n),s)
move' (x,y) s (S,n) = ((x,y+n),s)
move' (x,y) s (W,n) = ((x-n,y),s)
move' (x,y) s (E,n) = ((x+n,y),s)
move' w@(x,y) s (L,n)
    | n > 0 = move' (y,-x) s (L,n-90)
    | otherwise = (w,s)
move' w@(x,y) s (R,n)
    | n > 0 = move' (-y,x) s (R,n-90)
    | otherwise = (w,s)
move' w@(x,y) (x',y') (F,n) = (w, (x'+n*x, y'+n*y))
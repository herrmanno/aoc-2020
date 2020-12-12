import Data.Functor ((<&>))
import Data.Maybe (listToMaybe,fromMaybe,mapMaybe)
import Control.Monad.State (evalState, MonadState(put, get), State)

data Dir = N | E | S | W deriving (Show, Read, Enum)
data Command = Move Dir | L | R | F deriving (Show, Read)
type Instr = (Command, Int)

main = (readFile "input.txt" <&> lines) >>= (print . run)

run :: [String] -> Int
run xs = evalState move (0,0,E,xs') where xs' = map parse xs

parse :: String -> Instr
parse s = (readCommand [head s], read $ tail s) where
    readCommand s = head $ mapMaybe ($ s) [fmap Move . readDir,readLRFCommand]
    readDir = listToMaybe . map fst . reads :: String -> Maybe Dir
    readLRFCommand = listToMaybe . map fst . reads :: String -> Maybe Command

type S = (Int,Int,Dir,[Instr]) -- x,y,current direction, remaining instructions

move :: State S Int
move = do
    (x,y,d,is) <- get
    let (x',y',d') = move' x y d (fromMaybe (F,0) $ listToMaybe is)
    put (x',y',d',tail is)
    if null is then return (abs x' + abs y') else move

move' :: Int -> Int -> Dir -> Instr -> (Int,Int,Dir)
move' x y d (Move N,n) = (x,y-n,d)
move' x y d (Move S,n) = (x,y+n,d)
move' x y d (Move W,n) = (x-n,y,d)
move' x y d (Move E,n) = (x+n,y,d)
move' x y d (L,n)
    | n > 0 = move' x y (toEnum ((fromEnum d + 3) `mod` 4)) (L,n-90)
    | otherwise = (x,y,d)
move' x y d (R,n)
    | n > 0 = move' x y (toEnum ((fromEnum d + 1) `mod` 4)) (R,n-90)
    | otherwise = (x,y,d)
move' x y d (F,n) = move' x y d (Move d, n)
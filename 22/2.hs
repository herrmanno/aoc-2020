import Debug.Trace (traceShow)
import Control.Monad.State
main = readFile "input.txt" >>= print . stackSum . snd . (\(d1,d2) -> play (0,d1) (0,d2) []) . parse

type Game = (PlayerState,PlayerState)
type PlayerState = (SubGameCard, Deck)
type SubGameCard = Int
type Deck = [Int]

parse :: String -> ([Int],[Int])
parse s = let
    ls = lines s
    (ls1,_:ls2) = break (=="") ls
    [ls1',ls2'] = map (map read . drop 1) [ls1,ls2]
    in (ls1',ls2')

stackSum xs = sum $ zipWith (*) [1..] (reverse xs)

play ::
    (Int,[Int]) ->          -- player 1 (sub game card, deck)
    (Int,[Int]) ->          -- player 2 (sub game card, deck)
    [Game] ->               -- seen game states (for subgames)
    ([Int],[Int])           -- (winning cards (in order), final stack)
play (a,[]) (b,bs) _ = {- traceShow "Player 2 won by empty stack" -} ([b,a], bs)
play (a,as) (b,[]) _ = {- traceShow "Player 1 won by empty stack" -} ([a,b], as)
play (x,a:as) (y,b:bs) ss
    | nextState `elem` ss = {- traceShow "Player 1 won be recursive state" -} ([x,y], [])
    | a <= length as && b <= length bs = let
        as' = take a as
        bs' = take b bs
        (c@[c1,c2],_) = {- traceShow "Recursing into another game" $ -} play (a, take a as) (b, take b bs) []
        in if maximum as' > maximum bs'  -- IMPORTANT!
            then play (x,as++[a,b]) (y,bs) ss'
            else if c1 == a
                then {- traceShow "Player 1 won subgame" $ -} play (x, as ++ c) (y,bs) ss'
                else {- traceShow "Player 2 won subgame" $ -} play (x, as) (y, bs ++ c) ss'
    | a > b = {- traceShow ("Player 1 has higher card: " ++ show [a,b]) $ -} play (x,as++[a,b]) (y,bs) ss'
    | b > a = {- traceShow ("Player 2 has higher card: " ++ show [a,b]) $ -} play (x,as) (y,bs++[b,a]) ss'
    where
        nextState = ((x,a:as),(y,b:bs))
        ss' = nextState:ss
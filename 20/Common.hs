module Common (Tile, parse, parseTile, flipsRotated, constructImage, edgeIDs ) where

import Data.List (unfoldr, foldl', (\\), nub, reverse, transpose, )
import Data.Char (isDigit)
import Data.Bits as B ( Bits(bit, (.&.)) )
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace (trace)
import Control.Monad (forM_)

type Tile = (Int, [String])

tileDim = 12

-- used only as preparation for part 2
printImage :: Map (Int,Int) Tile -> IO ()
printImage m =  forM_ [0..(tileDim-1)] (\i ->
                    forM_ [0..9] (\r -> do
                        forM_ [0..(tileDim-1)] (\j -> putStr $ snd (m M.! (j,i)) !! r)
                        putStr "\n"
                    )
                )

parse :: String -> [Tile]
parse = unfoldr f where
    f s = let
        (s',xs) = break (=="") $ lines s
        xs' = if null xs then [] else tail xs
        in if null s' then Nothing else Just (parseTile s', unlines xs')

parseTile :: [String] -> Tile
parseTile (h:xs) = let
    id = read $ filter isDigit h
    in (id, xs)

rotations :: Tile -> [Tile]
rotations t = map (`rot` t) [0..3] where
    rot 0 t = t
    rot n (i,ts) = rot (n-1) (i, transpose $ map reverse ts)

flips :: Tile -> [Tile]
flips tile@(i,xs) = [ tile, (i, reverse xs), (i, map reverse xs) ]

flipsRotated :: Tile -> [Tile]
flipsRotated = nub . concatMap rotations . flips


edgeIDs :: Map (Int,Int) Tile -> [Int]
edgeIDs m = map fst [ m M.! (0,0), m M.! (0,tileDim-1), m M.! (tileDim-1, 0), m M.! (tileDim-1, tileDim-1)]

constructImage :: [Tile] -> Map (Int,Int) Tile
constructImage ts = constructImageBFS [(allVariants, (0,0), M.empty)] where
    allVariants = concatMap flipsRotated ts

constructImageBFS ::
    [([Tile], (Int,Int), Map (Int,Int) Tile)] ->    --  Queue of (remaining tiles, next index to fill, curr state)
    Map (Int,Int) Tile                              --  final state
constructImageBFS (([],_,m):_) = m
constructImageBFS [] = error "no final state reached"
constructImageBFS (s@(ts,idx,m):ss) = let
    ss' = ss -- trace ("\ESC[2J" ++ "States: " ++ (show $ length ss)) ss
    validTiles = filter (doesMatch s) ts
    newStates = map (\t -> (filter ((/= fst t) . fst) ts, nextIdx idx, M.insert idx t m)) validTiles
    in constructImageBFS (ss' ++ newStates)
    where
        nextIdx (x,y) = let x' = (x + 1) `rem` tileDim; y' = y + ((x+1) `div` tileDim) in (x',y')
        doesMatch :: ([Tile], (Int,Int), Map (Int,Int) Tile) -> Tile -> Bool
        doesMatch (_,(x,y),m) tile =
                edgesMatch (leftEdge $ Just $ snd tile) (rightEdge $ fmap snd $ m M.!? (x-1,y)) &&
                edgesMatch (topEdge $ Just $ snd tile) (bottomEdge $ fmap snd $ m M.!? (x,y-1))
        topEdge = fmap head
        bottomEdge = fmap last
        leftEdge = fmap (map head)
        rightEdge = fmap (map last)
        edgesMatch Nothing _ = True
        edgesMatch _ Nothing = True
        edgesMatch (Just a) (Just b) = a == b


-- inverts a binary number in 2^10 space e.g.
-- 0000000001 -> 1000000000; 
invert :: Int -> Int
invert 0 = 1023
invert i = sum $ map go [0..9] where
    go n = if (i .&. bit n) /= 0 then bit (9-n) else 0

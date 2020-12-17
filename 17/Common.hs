module Common ( parse, Dim(..), PocketDimension(..), evolve, evolveN ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List ( nub )
import Data.Maybe ( fromMaybe )

data Dim = Dim3 | Dim4

data Coord = Coord3 (Int,Int,Int) | Coord4 (Int,Int,Int,Int) deriving (Show, Ord, Eq)
type Cubes = Map Coord Bool 
newtype PocketDimension = PocketDimension { cubes :: Cubes } deriving (Show)

parse :: Dim -> String -> PocketDimension
parse dim s = PocketDimension (parseCubes dim s)

parseCubes :: Dim -> String -> Cubes
parseCubes dim s = foldr insertLine M.empty  $ zip [0..] $ lines s where
    insertLine (i,s') m = foldr (insertCol i) m $ zip [0..] s'
    insertCol i (j,c) m = case dim of
        Dim3 -> M.insert (Coord3 (j,i,0)) (c == '#') m
        Dim4 -> M.insert (Coord4 (j,i,0,0)) (c == '#') m

neighbours :: Coord -> [Coord]
neighbours (Coord3 (a,b,c)) = [
        Coord3 (a+x,b+y,c+z) |
        x <- [-1..1],
        y <- [-1..1],
        z <- [-1..1],
        (x,y,z) /= (0,0,0)
    ]
neighbours (Coord4 (a,b,c,d)) = [
    Coord4 (a+x,b+y,c+z,d+w) |
        x <- [-1..1],
        y <- [-1..1],
        z <- [-1..1],
        w <- [-1..1], (x,y,z,w) /= (0,0,0,0)
    ]

evolveN :: Int -> PocketDimension -> PocketDimension 
evolveN 0 pd = pd
evolveN n pd = evolveN (n-1) (evolve pd)

evolve :: PocketDimension -> PocketDimension 
evolve (PocketDimension cs) = PocketDimension (evolveCubes cs)

evolveCubes :: Cubes -> Cubes 
evolveCubes xs = foldr go xs (nub $ concatMap neighbours $ filter (xs M.!) $ M.keys xs) where
    go k m = let
        c = M.findWithDefault False k xs
        activeNeighbours = sum $ map (fromEnum . fromMaybe False . (M.!?) xs) $ neighbours k
        in if c
            then M.insert k (activeNeighbours `elem` [2,3]) m
            else M.insert k (activeNeighbours == 3) m
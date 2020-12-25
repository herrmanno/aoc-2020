import Text.Parsec ( string, many1, (<|>), parse, try )
import Data.Maybe ( fromMaybe )
import Data.Either ( fromRight )
import Data.Char ( toUpper )
import Data.List ( nub, minimum, maximum, intercalate )
import Data.Map ( Map )
import qualified Data.Map as M
import Control.Monad ( forM_ )

data Move = NW | NE | SW | SE | W | E deriving (Read,Show)

printTiles :: Map (Int,Int) Bool -> IO ()
printTiles m = do
    let ks = M.keys m
    let minX = -1 * max (maximum $ map fst ks) (abs (minimum $ map fst ks))
    let minY = -1 * max (maximum $ map snd ks) (abs (minimum $ map snd ks))
    putStrLn $ "\t" ++ concatMap (show . abs) [minX..(-1)*minX]
    forM_ [minY..(-1 * minY)] $ \y -> do
        let toTile t = case t of
                                (Just True) -> "#"
                                _ -> "."
        let x0
              | even y = if even minX then minX else minX - 1
              | even minX = minX - 1
              | otherwise = minX
        -- print tiles (# / .)
        let line = unwords $ map (\x -> toTile $ m M.!? (x,y)) [x0,(x0+2)..((-1) * x0)]
        -- print black neighbour tiles count
        -- let line = unwords $ map (\x -> show $ snd $ neighbourTiles (x,y) m) [x0,(x0+2)..((-1) * x0)]
        if even y
            then putStr (show y ++ "\t ")
            else putStr (show y ++ "\t")
        putStrLn line

main = readFile "input.txt" >>= (print . blackTiles . evolve 100 . createFloor)

evolve :: Int -> Map (Int,Int) Bool -> Map (Int,Int) Bool
evolve 0 m = m
evolve n m = let
    ns = nub $ concatMap neighbours (M.keys m) ++ M.keys m
    accumulate p m' = M.alter (alterTile p) p m'
    alterTile p (Just True) = let
        (_,b) = neighbourTiles p m
        in if b == 0 || b > 2 then Nothing else Just True
    alterTile p _ = let
        (_,b) = neighbourTiles p m
        in if b == 2 then Just True else Nothing
    in evolve (n-1) $ foldr accumulate m ns

neighbourTiles :: (Int,Int) -> Map (Int,Int) Bool -> (Int,Int)
neighbourTiles p m = let
    tiles = map (m M.!?) $ neighbours p
    f (Just True) (w,b) = (w,b+1)
    f _ (w,b) = (w+1,b)
    in foldr f (0,0) tiles

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) = [ (x-2,y),(x+2,y),(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1) ]

blackTiles :: Map (Int,Int) Bool -> Int
blackTiles = length . filter id . M.elems

createFloor :: String -> Map (Int,Int) Bool
createFloor = (`flipTiles` M.empty) . (map parseLine . lines)

parseLine :: String -> [Move]
parseLine  = fromRight [] . parse p "" where
    p = many1 parseMove
    parseMove = (read :: String -> Move) . map toUpper <$>
        (try (string "nw") <|> try (string "ne") <|> try (string "sw") <|> try (string "se") <|> string "w" <|> string "e")

flipTiles :: [[Move]] -> Map (Int,Int) Bool -> Map (Int,Int) Bool
flipTiles xs m = foldr (\x m -> M.alter (Just . not . fromMaybe False) (move x (0, 0)) m) m xs

move :: [Move] -> (Int,Int) -> (Int,Int)
move [] p = p
move (NE:xs) (x,y) = move xs (x+1,y-1)
move (NW:xs) (x,y) = move xs (x-1,y-1)
move (SE:xs) (x,y) = move xs (x+1,y+1)
move (SW:xs) (x,y) = move xs (x-1,y+1)
move (E:xs) (x,y) = move xs (x+2,y)
move (W:xs) (x,y) = move xs (x-2,y)
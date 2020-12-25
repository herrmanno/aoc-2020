import Text.Parsec
import Data.Maybe ( fromMaybe )
import Data.Either ( fromRight )
import Data.Char ( toUpper )
import Data.Map ( Map )
import qualified Data.Map as M

data Move = NW | NE | SW | SE | W | E deriving (Read,Show)

main = readFile "input.txt" >>= (print . length . filter id . M.elems . (`flipTiles` M.empty)) . (map parseLine . lines)

parseLine :: String -> [Move]
parseLine  = fromRight [] . parse p "" where
    p = many1 parseMove
    parseMove = (read :: String -> Move) . map toUpper <$>
        (try (string "nw") <|> try (string "ne") <|> try (string "sw") <|> try (string "se") <|> string "w" <|> string "e")

flipTiles :: [[Move]] -> Map (Int,Int) Bool -> Map (Int,Int) Bool
flipTiles [] m = m
flipTiles (x:xs) m = flipTiles xs $ M.alter (\v -> Just $ not $ fromMaybe False v) (move x (0,0)) m

move :: [Move] -> (Int,Int) -> (Int,Int)
move [] p = p
move (NE:xs) (x,y) = move xs (x+1,y-1)
move (NW:xs) (x,y) = move xs (x-1,y-1)
move (SE:xs) (x,y) = move xs (x+1,y+1)
move (SW:xs) (x,y) = move xs (x-1,y+1)
move (E:xs) (x,y) = move xs (x+2,y)
move (W:xs) (x,y) = move xs (x-2,y)
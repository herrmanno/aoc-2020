import Text.Parsec ((<|>))
import qualified Text.Parsec as P

data Dir = N | E | S | W deriving (Show, Read, Enum)
data Command = Move Dir | L | R | F deriving (Show, Read)
type Instr = (Command, Int)

main = readFile "input.txt" >>= (print . run . lines)

-- run :: [String] -> Int
run xs = move 0 0 E xs'
    where xs' = case parse xs of
            Right xs' -> xs'
            Left err -> error $ show err

parse :: [String] -> Either P.ParseError [Instr]
parse = P.parse parseInstructions ""  . unwords where
    parseInstructions = parseInstruction `P.sepEndBy1` P.space
    parseInstruction = (,) <$> parseCommand <*> parseNum
    parseCommand = P.try (parseMove <|> parseLFRCommand)
    parseMove = Move <$> (read <$> P.many1 (P.oneOf "NESW"))
    parseLFRCommand = read <$> P.many1 (P.oneOf "LRF")
    parseNum = read <$> P.many1 P.digit

move :: Int -> Int -> Dir -> [Instr] -> Int
move x y _ [] = abs x + abs y
move x y d ((Move N,n):xs) = move x (y-n) d xs
move x y d ((Move S,n):xs) = move x (y+n) d xs
move x y d ((Move W,n):xs) = move (x-n) y d xs
move x y d ((Move E,n):xs) = move (x+n) y d xs
move x y d ((L,n):xs)
    | n > 0 = move x y (toEnum ((fromEnum d + 3) `mod` 4)) ((L,n-90):xs)
    | otherwise = move x y d xs
move x y d ((R,n):xs)
    | n > 0 = move x y (toEnum ((fromEnum d + 1) `mod` 4)) ((R,n-90):xs)
    | otherwise = move x y d xs
move x y d ((F,n):xs) = move x y d ((Move d, n):xs)

module Common ( splitInput, getResult, parseRules1, parseRules2 ) where

import Data.List ((\\))
import qualified Data.Map as M
import Text.Parsec as P
    ( anyChar,
      char,
      digit,
      newline,
      string,
      between,
      count,
      many1,
      option,
      sepBy,
      sepEndBy,
      (<|>),
      try,
      ParsecT )

import qualified Data.Functor.Identity
import Data.Maybe (fromJust, isJust)
import Control.Applicative (Applicative(liftA2))

type InputParser = ParsecT String () Data.Functor.Identity.Identity String
type ParserMap = M.Map Int InputParser
type Rule = (Int, Either Char ([Int], [Int]))

splitInput :: [Char] -> (String, [[Char]])
splitInput s = let (a,b) = span (/="") (lines s) in (unlines a, tail b)

getResult :: Show a => Either a p -> p
getResult (Left err) = error $ show err
getResult (Right x) = x

type RuleParser = ParsecT String () Data.Functor.Identity.Identity InputParser

parseRules1 :: RuleParser
parseRules1 = parseRules buildParser

parseRules2 :: RuleParser
parseRules2 = parseRules buildParser'

parseRules :: BuildParser -> RuleParser
parseRules buildParser = do
    rules <- many1 parseRule
    let m = M.empty :: ParserMap
    let parserMap = buildParserMap buildParser m rules
    let rootParser = parserMap M.! 0
    return rootParser

parseRule :: ParsecT String () Data.Functor.Identity.Identity Rule
parseRule = do
    id <- num
    string ": "
    c <- option Nothing $ Just <$> try (between (char '"') (char '"') anyChar)
    rules <- option Nothing $ Just <$> ((,) <$> (num `sepEndBy` char ' ') <*> option [] (string "| " *> (num `sepBy` char ' ')))
    newline
    if isJust c
        then return (id, Left $ fromJust c)
        else return (id,  Right $ fromJust rules)
    where num = (read :: String -> Int) <$> many1 digit

type BuildParserMap = ParserMap -> [Rule] -> ParserMap

buildParserMap :: BuildParser -> BuildParserMap
buildParserMap _ m [] = m
buildParserMap buildParser m (x@(i,_):xs)
    | all (`M.member` m) (ids x) = buildParserMap buildParser (M.insert i (buildParser x m) m) xs
    | otherwise = buildParserMap buildParser m (xs++[x])

type BuildParser = Rule -> ParserMap -> InputParser

buildParser :: BuildParser
buildParser (i, Left c) m = (:[]) <$> char c
buildParser (i, Right (xa,xb)) m =
    let pas = map (m M.!) xa
        pbs = map (m M.!) xb
        pa = foldr (liftA2 (++)) (pure "") pas
        pb = foldr (liftA2 (++)) (pure "") pbs
        label = show i
    in if null xb then try pa else try pa <|> try pb

buildParser' :: BuildParser
buildParser' (0, _) m = do
    let p42 = m M.! 42
    let p31 = m M.! 31
    let p8 n = concat <$> try (count n p42)
    let p11 n
            | n == 1 = (++) <$> try p42 <*> try p31
            | otherwise = (+++) <$> try p42 <*> try (p11 (n-1)) <*> try p31
            where (+++) a b c = a ++ b ++ c
    let p0 n m
            | n > 5 || m > 5 = fail "Stackoverflow!" -- choosen by trying - must be great enough to catch all samples but increaes
            | otherwise = try ((++) <$> try (p8 n) <*> try (p11 m)) <|> try (p0 n (m+1)) <|> try (p0 (n+1) m) 
    p0 1 1
buildParser' r m = buildParser r m

ids :: Rule -> [Int]
ids (_,Left _) = []
ids (i,Right (xa,xb)) = (xa ++ xb) \\ [i]
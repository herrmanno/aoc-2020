module Common ( solve ) where

import Text.ParserCombinators.ReadP ( (<++) )
import qualified Text.ParserCombinators.ReadP as P
import Data.Maybe ( listToMaybe )

solve :: (Show a) => ([String] -> a) -> String -> Maybe Int
solve f = fmap (read . fst) . listToMaybe . P.readP_to_S (solveEquation f)

solveEquation :: Show a => ([String] -> a) -> P.ReadP String
solveEquation f = solve <$> parseEquation where
    solve = show . f. words
    parseEquation = parseStr <++ parseBracket <++ P.string ""
    parseStr = (++) <$> P.munch1 (not . (`elem` "()")) <*> parseEquation
    parseBracket = (++) <$> P.between (P.char '(') (P.char ')') (solveEquation f) <*> parseEquation


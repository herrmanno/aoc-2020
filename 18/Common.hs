{-# LANGUAGE FlexibleContexts #-}

module Common ( solve ) where

import Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

solve f s = case P.parse (solveEquation f)  "" s of
    Left err -> error $ show err
    Right ex -> read ex

solveEquation f = show . f . words . concat <$> P.many1 ( P.try ( parseEx f <|> parseS ) ) where
    parseS = P.many1 ( P.noneOf "()" <|> P.space )
    parseEx f = (P.char '(' *> solveEquation f) <* P.char ')'

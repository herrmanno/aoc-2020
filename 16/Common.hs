module Common
    ( parse
    , ParseResult(..)
    , Rule(..)
    , Ticket(..)
    )
where

import qualified Text.Parsec as P

type Interval = (Int,Int)

data Rule = Rule { ruleName :: String, i1 ::  Interval, i2 :: Interval } deriving (Eq)

instance Show Rule where
    show = ruleName

newtype Ticket = Ticket { ticketValues :: [Int] } deriving (Show)

data ParseResult = ParseResult
    { rules :: [Rule]
    , yourTicket :: Ticket
    , nearbyTickets :: [Ticket]
    } deriving (Show)



parse :: String -> ParseResult
parse s = case P.parse parseInput "" s of
            (Left err) -> error $ show err
            (Right r) -> r

parseInput = do
    rules <- concat <$> parseRules
    P.newline 
    P.string "your ticket:" >> P.newline 
    yourTicket <- parseTicket <* P.newline 
    P.newline 
    P.string "nearby tickets:"  >> P.newline 
    nearbyTickets <- parseTicket `P.sepEndBy` P.newline

    return ParseResult { rules = rules, yourTicket = yourTicket, nearbyTickets = nearbyTickets}

parseRules = P.manyTill (parseRule <* P.newline) (P.lookAhead P.newline) where
    parseRule = do
        name <- P.many1 (P.noneOf ":") <* P.string ": "
        (a,b) <- parseInterval
        (c,d) <- P.try (P.string " or " *> parseInterval)
        return [Rule name (a,b) (c,d)]
    parseInterval = do
        a <- read <$> P.many1 P.digit
        P.char '-'
        b <- read <$> P.many1 P.digit
        return (a,b)
    

parseTicket = Ticket . map read <$> P.many1 P.digit `P.sepBy` P.char ','
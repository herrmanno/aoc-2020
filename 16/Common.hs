module Common
    ( parse
    , ParseResult(..)
    , Rule(..)
    , ruleName
    , Ticket(..)
    , ticketValues
    )
where

import qualified Text.Parsec as P

type Interval = (Int,Int)

data Rule = Rule String Interval Interval deriving (Eq)

instance Show Rule where
    show (Rule name _ _) = name

ruleName (Rule name _ _) = name

newtype Ticket = Ticket [Int] deriving (Show)

ticketValues (Ticket t) = t

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
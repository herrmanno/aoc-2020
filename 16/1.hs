import Data.Coerce (coerce)
import Common
    ( parse
    , Rule(..)
    , Ticket(..)
    , ParseResult(..)
    )

main = readFile "input.txt" >>= (print . run . parse)

run :: ParseResult -> Int
run ParseResult{ rules=rs, nearbyTickets=ts } = sum $ filter invalid $ concatMap coerce ts where
    invalid i = not $ any (inRule i) rs
    inRule i (Rule _ (a,b) (c,d)) = i >= a && i <= b || i >= c && i <= d
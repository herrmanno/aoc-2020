import Data.Function (on)
import Data.List ((\\), isPrefixOf)
import Common
    ( parse
    , Rule(..)
    , Ticket(..)
    , ParseResult(..)
    )

main = readFile "input.txt" >>= (print . run . parse)

run :: ParseResult -> Int
run ParseResult{ rules=rs, yourTicket=(Ticket yt), nearbyTickets=ts } = product departureValues where
    departureValues = map snd $ filter (("departure" `isPrefixOf`) . fst) $ zip sortedRuleNames yt
    sortedRuleNames = map ruleName $ concat $ solveRules rulesOptions
    rulesOptions = foldr f allRules validTickets where
        f (Ticket ts) rs = zipWith (filter . inRule) ts rs
    allRules = map (const rs) yt
    validTickets = filter (not . any invalid . ticketValues) ts
    invalid i = not $ any (inRule i) rs
    inRule i (Rule _ (a,b) (c,d)) = i >= a && i <= b || i >= c && i <= d

solveRules :: [[Rule]] -> [[Rule]]
solveRules rs = let
                solved = solve rs
                solve rs = [
                        foldr (flip (\\)) r [r' | (j,r') <- zip [0..]rs, j /= i && length r' == 1]
                        | (i,r) <- zip [0..] rs
                    ]
                in  if ((==) `on` (length . concat)) solved rs
                        then rs
                        else solveRules solved
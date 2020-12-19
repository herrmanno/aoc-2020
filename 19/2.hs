import Text.Parsec ( parse, eof )
import Data.Either ( rights,lefts )
import Common
    ( splitInput
    , getResult
    , parseRules2
    )

main = do
    input <- readFile "input.txt"
    let (rules, values) = splitInput input
    let rootParser = getResult $ parse parseRules2 "" rules
    let result = rights $ map (parse (rootParser <* eof) "") values
    print $ length result
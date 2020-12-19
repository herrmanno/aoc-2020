import Text.Parsec ( parse, eof )
import Data.Either ( rights )
import Common
    ( splitInput
    , getResult
    , parseRules1
    )

main = do
    input <- readFile "input.txt"
    let (rules, values) = splitInput input
    let rootParser = getResult $ parse parseRules1 "" rules
    let result = length $ rights $ map (parse (rootParser <* eof) "") values
    print result
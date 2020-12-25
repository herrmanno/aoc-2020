import Common ( result, play, parse )
import Data.List ( intercalate )

main = readFile "input.txt" >>= putStrLn . intercalate "" . map show . result 1 8 . play 100 . parse 9
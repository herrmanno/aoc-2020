import Text.Parsec as P
import Common ( solve )

main = readFile "input.txt" >>= print . sum . map (solve calc) . lines

calc :: [String] -> Int
calc = calc' . reverse

calc' :: [String] -> Int
calc' [a] = read a
calc' (a:"+":b) = read a + calc' b
calc' (a:"*":b) = read a * calc' b
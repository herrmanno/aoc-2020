import Common ( parse, result, play )

main = readFile "input.txt" >>= print . product . result 1 2 . play 10000000 . parse 1000000
import Common ( parse, constructImage, edgeIDs )

main = readFile "input.txt" >>= print . product . edgeIDs . constructImage . parse
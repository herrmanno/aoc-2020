main = readFile "input.txt" >>= print . stackSum . uncurry play . parse

parse :: String -> ([Int],[Int])
parse s = let 
    ls = lines s
    (ls1,_:ls2) = break (=="") ls
    [ls1',ls2'] = map (map read . drop 1) [ls1,ls2]
    in (ls1',ls2')

stackSum xs = sum $ zipWith (*) [1..] (reverse xs)

play :: [Int] -> [Int] -> [Int]
play [] b = b
play a [] = a
play x@(a:as) y@(b:bs)
    | a > b = play (as++[a,b]) bs
    |otherwise = play as (bs++[b,a])
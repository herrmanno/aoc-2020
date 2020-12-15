input = [11,0,1,10,5,19]

main = print $ result !! 2019

result = input ++ [number n | n <- [length input..]] where
    number n =  let xs = zip [0..] (take n result)
                    recent = snd $ last xs
                    recents = filter ((==recent) . snd) xs
                    count = length recents
                    [da,db] = map fst $ drop (length recents -2) recents
                    diff = abs $ da - db
                in case count of
                    1 -> 0
                    _ -> diff
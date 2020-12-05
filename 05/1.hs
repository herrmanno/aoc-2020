{-# LANGUAGE ViewPatterns #-}

main = do
    input <- lines <$> readFile "input.txt"
    let result = run input
    putStrLn $ show result

run :: [String] -> Int
run xs = maximum $ map seatId $ positions xs where
    seatId (r,c) = 8 * r + c

positions :: [String] -> [(Int,Int)]
positions xs = map go xs where
    go (splitAt 7 -> (xs1,xs2)) = (nav xs1, nav xs2)

nav :: String -> Int
nav = nav' 0 where
    nav' :: Int -> String -> Int
    nav' i [] = i
    nav' i (x:xs)
        | x == 'B' || x == 'R' = nav' (i + 2^(length xs)) xs
        | otherwise = nav' i xs


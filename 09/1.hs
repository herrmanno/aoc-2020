main = do
    input <- map (read) <$> lines <$> readFile "input.txt"
    let result = run 25 input
    putStrLn $ show result

run :: Int -> [Int] -> Maybe Int
run  n xs = let x = xs !! n
                preamble = filter ((/=)x . (*)2) $ take n xs
                summable = any (\m -> abs (m-x) `elem` preamble) preamble
            in if summable then run n (tail xs) else Just x

main = do
    text <- readFile "input.txt"
    let input = map read $ lines text
    let result = run input
    putStrLn $ show result

run :: [Int] -> Int
run xs = last $ head xs'
    where xs' = [
                    [a,b,a*b*c] |
                    a <- xs,
                    b <- dropWhile (/=a) xs,
                    c <- dropWhile (/=b) xs,
                    a+b+c == 2020
                ]

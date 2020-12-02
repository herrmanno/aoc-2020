main = do
    text <- readFile "input.txt"
    let input = map read $ lines text
    let result = run input
    putStrLn $ show result

run :: [Int] -> Int
run xs = last $ head xs'
    where xs' = [[a,b,a*b] | a <- xs, b <- dropWhile (/=a) xs, a+b == 2020]

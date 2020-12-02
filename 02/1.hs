main = do
    text <- readFile "input.txt"
    let input = map parseLine $ lines text
    let result = run input
    --putStrLn $ show $ take 5 input
    putStrLn $ show result

type Line = ((Int,Int),Char,String)

parseLine :: String -> Line
parseLine xs = ((read mi, read ma), ch, pw)
    where [limits, (ch:_), pw] = words xs
          (mi,(_:ma)) = span (/='-') limits

run :: [Line] -> Int
run = length . filter p
    where p ((mi,ma), ch, pw) =
            let s = length $ filter (==ch) pw in mi <= s && s <= ma

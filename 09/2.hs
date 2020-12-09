import Data.List (elemIndex)
import Data.Maybe (fromJust)

main = do
    input <- map (read) <$> lines <$> readFile "input.txt"
    let n = 675280050
    let k = fromJust $ elemIndex n input
    let result = run n k input
    putStrLn $ show result

run n k xs = go 0 0 (head xs) where
    go a b s = case s `compare` n of
                    LT -> go a (b+1) (s + (xs !! (b+1)))
                    GT -> go (a+1) b (s - (xs !! a))
                    otherwise -> let ys = map (xs !!) [a..b] in minimum ys + maximum ys

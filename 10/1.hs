import Data.List (sort)
main = do
    input <- map (read) <$> lines <$> readFile "input.txt"
    let result = run input
    print result

run :: [Int] -> Int
run xs = let xs' = sort (0:(maximum xs + 3):xs)
             diffs = sort $ zipWith (subtract) xs' (tail xs')
             numjolts n = length $ filter (==n) diffs
         in numjolts 1 * numjolts 3

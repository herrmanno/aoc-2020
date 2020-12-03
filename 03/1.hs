main = do
    input <- lines <$> readFile "input.txt"
    let result = run (0,0) (3,1) input
    putStrLn $ show result

run :: (Int,Int) -> (Int,Int) -> [String] -> Int
run (x,y) (dx,dy) xss = foldr f 0 fields
    where   f '#' a = a + 1
            f _ a = a
            steps = iterate (\(x,y) -> (x+dx,y+dy)) (x,y)
            fields = zipWith (\(x,_) ls -> ls !! (x `mod` (length ls))) steps xss 

main = do
    input <- lines <$> readFile "input.txt"
    let result = product $ map (run' input) slopes
    putStrLn $ show result

slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]

run' :: [String] -> (Int,Int) -> Int
run' ls d = run (0,0) d ls

run :: (Int,Int) -> (Int,Int) -> [String] -> Int
run (x,y) (dx,dy) xss = foldr f 0 fields
    where   f '#' a = a + 1
            f _ a = a
            numSteps = (round $ (fromIntegral $ length xss) / (fromIntegral dy)) :: Int
            steps = iterate (\(x,y) -> (x+dx,y+dy)) (x,y)
            fields = map get $ take numSteps steps
            get (x,y) = let xs = xss !! (y `mod` (length xss)) in  xs !! (x `mod` (length xs))

main = readFile "input.txt" >>= (print . run . lines)

run :: [String] -> Int
run (_:xs:_) = let  xs' = parse xs
                    r = foldr1 f xs'
               in   fst r

f :: (Int,Int) -> (Int,Int) -> (Int,Int)
f (b,m) (a,n) = let x = head [x | x <- [a,a+n..n*m], x `mod` n == a && x `mod` m == b] in (x,n*m)

parse :: String -> [(Int,Int)] -- [(a,b),…] meaning x mod b must be a
parse xs =  let xs' = words $ [if c == ',' then ' ' else c | c <- xs]
                zs = zip [0..] xs'
                pairs = filter (not . null . snd) $ map (\(i,n) -> (i, reads n)) $ zs
            in  map (\(i,ns) -> let n = (fst $ head ns) in ((n - i) `mod` n, n)) $ pairs
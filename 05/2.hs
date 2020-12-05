{-# LANGUAGE ViewPatterns #-}
import Data.List (sort)

main = do
    input <- lines <$> readFile "input.txt"
    let result = run input
    putStrLn $ show result

run :: [String] -> Int
run xs = (+) 1 $ fst $ head $ filter ((/=(-1)) . uncurry (-)) $ zip ids $ drop 1 ids where
    ids = sort $ map seatId $ positions xs
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


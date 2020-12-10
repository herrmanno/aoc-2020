{-# LANGUAGE ViewPatterns #-}

import Data.List (unfoldr, sort,nub)

type Range = [Int] -- Lower Bound, Variables, Upper Bound

main = do
    input <- map read <$> lines <$> readFile "input.txt"
    let result = run input
    print result

run xs = let xs' = sort ((-3):0:(maximum xs + 3):(maximum xs + 6):xs)
             diffs = zipWith subtract xs' (tail xs')
             zs = zip xs' diffs
          in product $ map combCount $ toRanges zs

toRanges :: [(Int,Int)] -> [Range]
toRanges = unfoldr f where
    f xs =  let (x,xs') = span ((==1) . snd) $ dropWhile ((==3) . snd) xs
            in if null xs' then Nothing else Just (map fst (x++[head xs']), xs')

combCount :: Range -> Int
combCount = length . filter valid . combinations

combinations :: Range -> [[Int]]
combinations xs = nub [l:var'++[h] | var' <- combs var ++ [[]]] where
    l = head xs
    h = last xs
    var = (tail . init) xs
    
combs :: [a] -> [[a]]
combs [] = [[]]
combs (x:xs) = map (x:) (combs xs) ++ combs xs

valid :: Range -> Bool
valid xs = all (<4) $ zipWith subtract xs (tail xs)
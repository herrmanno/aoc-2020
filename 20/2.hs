import Common ( Tile, parseTile, flipsRotated )
import qualified Data.List as L

main = do
    input <- readFile "part1.out"
    let tile@(_,ls) = parseTile $ (("Tile 1":) . lines) $ input
    let n = maximum $ map seaMonsterCount $ flipsRotated tile
    let total = length $ filter (=='#') $ concat ls
    print $ total - (n * 15)

seaMonsterCount :: Tile -> Int 
seaMonsterCount (_,xs) = length $ filter id [ seaMonsterAt i j xs | i <- [0..length xs], j <- [0..length xs] ]

{-
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
-}
seaMonsterAt :: Int -> Int -> [String] -> Bool
seaMonsterAt x y ls = let
    l1 = drop x $ first $ drop y ls
    l2 = drop x $ first $ drop (y+1) ls
    l3 = drop x $ first $ drop (y+2) ls
    in
        l1 `at` 18 == Just '#' &&
        all ((== Just '#') . (l1 `at`)) [0,5,6,11,12,17,18,19] && 
        all ((== Just '#') . (l2 `at`)) [1,4,7,10,13,16]
    where
        first [] = []
        first (x:xs) = x
        x `at` y = if y < length x then Just (x !! y) else Nothing
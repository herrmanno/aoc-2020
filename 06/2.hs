{-# LANGUAGE ViewPatterns #-}

import Data.List (sort,group,intercalate,intersect)
import Data.List.Split (splitOn)

main = do
    input <- readFile "input.txt"
    let result = run input
    putStrLn $ show result

run = sum . map (length . foldr1 intersect . lines) . splitOn "\n\n"


{-# LANGUAGE ViewPatterns #-}

import Data.List (sort,group)
import Data.List.Split (splitOn)

main = do
    input <- readFile "input.txt"
    let result = run input
    putStrLn $ show result

run = sum . map (length . group . sort . filter (/='\n')) . splitOn "\n\n"


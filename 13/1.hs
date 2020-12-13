import Data.Function (on)
import Data.List (minimumBy)
import Data.Char (isDigit)

main = readFile "input.txt" >>= (print . run . lines)

run :: [String] -> Int
run xs = let    (t, ids) = parse xs
                rest i = fromIntegral (ceiling (t / i)) - (t / i) 
                diffs = zip ids $ map rest ids
                m = fst $ minimumBy (compare `on` snd) diffs
                nextStop = head $ dropWhile (<t) [m,2*m..]
                mdiff = nextStop - t
            in  floor $ m * mdiff

parse :: [String] -> (Float,[Float])
parse (x:xs:_) = (read x, map read $ words $ [if isDigit c then c else ' ' | c <- xs])
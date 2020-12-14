import Data.Map (Map)
import qualified Data.Map as M
import Data.Bits (testBit)

type Mask = [Char]
type Address = [Char]
type Memory = Map Int Integer

main = readFile "input.txt" >>= (print . run . lines)

run = M.foldr (+) 0 . snd . run' "" M.empty
-- run = snd . run' "" M.empty

run' :: Mask -> Memory -> [String] -> (Mask,Memory)
run' mask mem [] = (mask,mem)
run' mask mem (x:xs) = let [k,v] = split x in case k of
    "mask" -> run' v mem xs
    _ ->    let address = (read $ init $ drop 4 k)
                addressBin = decToBin address
                addressesBin = calcAddresses mask addressBin
                addresses = map binToDec addressesBin
                value = read v
                mem' = foldr (`M.insert` value) mem addresses
            in  run' mask mem' xs

split :: String -> [String]
split xs = words $ [if c == '=' then ' ' else c | c <- xs]

calcAddresses :: Mask -> Address -> [Address]
calcAddresses m a = calcAddresses' $ zip m a where
    calcAddresses' :: [(Char,Char)] -> [Address]
    calcAddresses' [] = [[]]
    calcAddresses' ((m,a):xs) = case m of
        '0' -> map (a:) $ calcAddresses' xs
        '1' -> map ('1':) $ calcAddresses' xs
        'X' -> map ('1':) (calcAddresses' xs) ++ map ('0':) (calcAddresses' xs)

decToBin :: Int -> [Char]
decToBin n = reverse $ concatMap (show . fromEnum . testBit n) [0..35]

binToDec :: [Char] -> Int
binToDec xs = foldr f 0 $ zip [0..] (reverse xs) where
    f (i,c) acc = case c of '0' -> acc
                            '1' -> acc + 2^i
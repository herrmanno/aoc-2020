import Data.Map (Map)
import qualified Data.Map as M
import Data.Bits (setBit,clearBit)

type Mask = [Char]
type Memory = Map Int Integer

main = readFile "input.txt" >>= (print . run . lines)

run = M.foldr (+) 0 . snd . run' "" M.empty

run' :: Mask -> Memory -> [String] -> (Mask,Memory)
run' mask mem [] = (mask,mem)
run' mask mem (x:xs) = let [k,v] = split x in case k of
    "mask" -> run' v mem xs
    _      -> let address = (read $ init $ drop 4 k)
                  value = read v
                  value' = calc mask value
                  mem' = M.insert address value' mem
              in  run' mask mem' xs

calc :: Mask -> Integer -> Integer
calc m v = let m' = zip [0..] $ reverse m in foldr f v m' where
    f :: (Int,Char) -> Integer -> Integer
    f (_,'X') a = a
    f (i,'1') a = a `setBit` i
    f (i,'0') a = a `clearBit` i

split :: String -> [String]
split xs = words $ [if c == '=' then ' ' else c | c <- xs]
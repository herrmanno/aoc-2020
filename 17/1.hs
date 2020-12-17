import Common ( parse, evolveN, cubes, Dim(..) )

import qualified Data.Map as M

main = readFile "input.txt" >>= print . (sum . M.map fromEnum . cubes) . evolveN 6 . parse Dim3

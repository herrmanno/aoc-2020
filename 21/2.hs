import Common ( Food(..), Allergeen, Ingredients, parseFood, findPossibleAllergeens)
import Data.List (sort, intercalate, (\\))
import Data.Map (Map)
import qualified Data.Map as M

main = do
    input <- readFile "input.txt"
    let foods = map parseFood $ lines input
    let allergeenMap = findPossibleAllergeens foods
    putStrLn $ canonicalDangerousIngredientList (reduce allergeenMap)

canonicalDangerousIngredientList :: Map Allergeen Ingredients -> String
canonicalDangerousIngredientList m = intercalate "," $ map (head . (m M.!)) (sort $ M.keys m)

reduce :: Map Allergeen Ingredients -> Map Allergeen Ingredients
reduce m
    | all ((==1) . length) (M.elems m) = m
    | otherwise = reduce $ M.map (\is -> if length is == 1 then is else is \\ singleIngredients) m
    where
        singleIngredients = concat $ filter ((==1) . length) (M.elems m)
import Common ( Food(..), parseFood, safeIngredients)

main = do
    input <- readFile "input.txt"
    let foods = map parseFood $ lines input
    let si = safeIngredients foods
    let allIngredients = concatMap (\(Food is _) -> is) foods
    print $ length $ filter (`elem` si) allIngredients
module Common ( Food(..), Ingredients, Allergeen, parseFood, safeIngredients, ingredients, findPossibleAllergeens) where

import Text.Parsec
    ( alphaNum,
      space,
      string,
      between,
      many1,
      option,
      sepEndBy,
      parse )
import qualified Data.Either as E
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (intersect,nub,(\\))

type Ingredients = [String]
type Allergeen = String
type Allergens = [Allergeen]
data Food = Food Ingredients Allergens deriving (Show)

parseFood :: String -> Food
parseFood = E.fromRight (Food [] []) . parse food "" where
    food = do
            ingredients <- word `sepEndBy` space
            allergeens <- option [] $ between (string "(contains ") (string ")") (word `sepEndBy` string ", ")
            return $ Food ingredients allergeens
    word = many1 alphaNum

safeIngredients :: [Food] -> [String]
safeIngredients fs = ingredients fs \\ containingAllergeens fs

ingredients :: [Food] -> Ingredients
ingredients = nub . concatMap f where f (Food is _) = is

containingAllergeens :: [Food] -> Ingredients
containingAllergeens fs = let m = findPossibleAllergeens fs in nub . concatMap (m M.!) $ M.keys m

findPossibleAllergeens :: [Food] -> Map Allergeen Ingredients
findPossibleAllergeens = foldr go M.empty where
    go (Food is as) m = foldr (go' is) m as
    go' is a m = M.alter (Just . intersect is . fromMaybe is) a m
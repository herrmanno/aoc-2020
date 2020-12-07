import qualified Data.Map as M
import Data.List.Split (splitOn)

type Color = String
type Rule = (Color,Int)
type Line = (Color, (M.Map Color Int))
type RuleSet = M.Map Color Int
type Data = M.Map Color RuleSet

main = do
    input <- lines <$> readFile "input.txt"
    let m = M.fromList $ map parseLine input
    let n = contains m "shiny gold"
    putStrLn $ show $ (n - 1)

contains :: Data -> Color -> Int
contains xs c = 1 + (sum $ map (\k -> (rules M.! k) * (contains xs k)) $ M.keys rules) where
    rules = xs M.! c

parseLine :: String -> Line
parseLine xs =
    let c = unwords $ fst $ span (/="bags") $ words xs
        rulesStr = unwords $ tail $ snd $ span (/="contain") $ words xs
        rules = if ("no" `elem` (words rulesStr)) then [] else map parseRule $ splitOn ", " rulesStr
        in (c, M.fromList rules)

parseRule :: String -> Rule
parseRule xs =
    let n = read $ head $ words xs
        c = unwords $ tail $ init $ words xs
        in (c,n)
    


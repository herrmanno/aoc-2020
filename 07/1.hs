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
    let m' = map (canContain m "shiny gold") $ M.keys m
    let n = sum $ map fromEnum m'
    putStrLn $ show $ n

canContain :: Data -> Color -> Color -> Bool
canContain xs s c = case (M.lookup c xs) of (Just rs) -> M.member s rs || or (map (canContain xs s) (M.keys rs))
                                            Nothing -> False
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
    


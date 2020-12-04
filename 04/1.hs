type Passport = [String] -- list of field names

main = do
    input <- lines <$> readFile "input.txt"
    let result = run input
    putStrLn $ show result

run :: [String] -> Int
run xs = length $ filter valid $ parsePassports xs

parsePassports :: [String] -> [Passport]
parsePassports [] = []
parsePassports xs = parse ps : parsePassports pss'
    where   (ps,pss) = span (/="") xs
            pss' = if (>1) (length pss) then tail pss else []

parse :: [String] -> Passport
parse xs = foldr f [] $ concatMap words xs
    where f w a = (take 3 w) : a

valid :: Passport -> Bool
valid p = all (`elem` p) fields
    where fields = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

import Data.Char (isDigit)

type Passport = [(String,String)] -- list of (key,value) tuples

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
    where f w a = let (k,(_:v)) = span (/=':') w in (k,v):a

valid :: Passport -> Bool
valid passp = all id $ map test fields
    where   fields = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]
            keys = map fst passp
            val f = snd $ head $ (filter ((==f) . fst) passp)
            valMaybe f = case f `elem` keys  of False -> Nothing
                                                True -> Just $ val f
            test f = case valMaybe f of   Nothing -> False
                                          (Just v) -> testf f v
            testf "byr" v = every [(>=1920),(<=2002)] $ read v
            testf "iyr" v = every [(>=2010),(<=2020)] $ read v
            testf "eyr" v = every [(>=2020),(<=2030)] $ read v
            testf "hgt" v = let (n,s) = span isDigit v
                            in case s of "cm" -> every [(>=150),(<=193)] $ read n
                                         "in" -> every [(>=59),(<=76)] $ read n
                                         otherwise -> False
            testf "hcl" v = (head v == '#') && (and $ map (\c -> isDigit c || elem c "abcdef") $ tail v)
            testf "ecl" v = elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            testf "pid" v = (length v == 9) && (and $ map isDigit v)
            testf _ _ = True

every ps v = and $ (sequence ps) $ v
some ps v = or $ (sequence ps) $ v

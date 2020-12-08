import Control.Monad.State

type Code = (String,Int)
type CodeState = ([Code], Int {- IP -}, Int {- ACC -}, [Int] {- IP history -})

main = do
    input <- lines <$> readFile "input.txt"
    let result = run input
    putStrLn $ show result

run :: [String]  -> Int
run = runCode . parseCode

runCode :: [Code] -> Int
runCode c = evalState step (c,0,0,[])

step :: State CodeState Int
step = do
    (c,ip,acc,h) <- get
    let (instr,val) = c !! ip
    case instr of "nop" -> put (c,ip+1,acc,ip:h)
                  "acc" -> put (c,ip+1,acc+val,ip:h)
                  "jmp" -> put (c,ip+val,acc,ip:h)
    if ip `elem` h then return acc else step


parseCode :: [String] -> [Code]
parseCode = map (toTuple . words) where
    toTuple = (\(a:b:_) -> (a, readVal b))
    readVal ('+':s) = read s
    readVal ('-':s) = -1 * (read s)

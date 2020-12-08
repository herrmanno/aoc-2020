import Control.Monad.State
import Control.Monad (msum)

type Code = (String,Int)
type CodeState = ([Code], Int {- IP -}, Int {- ACC -}, [Int] {- IP history -})

main = do
    input <- lines <$> readFile "input.txt"
    let result = run input
    putStrLn $ show result

run :: [String]  -> Maybe Int
run = msum . (map runCode) . variants . parseCode

runCode :: [Code] -> Maybe Int
runCode c = evalState step (c,0,0,[])

variants :: [Code] -> [[Code]]
variants xs = [ flipOp n xs | n <- [0..(length xs - 1)] ]

flipOp :: Int -> [Code] -> [Code]
flipOp n xs = take n xs ++ [f (xs !! n)] ++ drop (n+1) xs where
    f ("nop",v) = ("jmp",v)
    f ("jmp",v) = ("nop",v)
    f c = c

step :: State CodeState (Maybe Int)
step = do
    (c,ip,acc,h) <- get
    let (instr,val) = c !! ip
    case instr of "nop" -> put (c,ip+1,acc,ip:h)
                  "acc" -> put (c,ip+1,acc+val,ip:h)
                  "jmp" -> put (c,ip+val,acc,ip:h)
    if ip `elem` h then return Nothing else
        (if ip + 1 == length c then return (Just acc) else step)


parseCode :: [String] -> [Code]
parseCode = map (toTuple . words) where
    toTuple = (\(a:b:_) -> (a, readVal b))
    readVal ('+':s) = read s
    readVal ('-':s) = -1 * (read s)

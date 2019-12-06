type ROM = [Integer]

splitString :: String -> [String]
splitString ([]) = []
splitString a = let curr = (splitStringLoop a [])
    in
        if null (snd curr) then splitString (fst curr)
        else (snd curr):(splitString (fst curr))

splitStringLoop :: String -> String -> (String, String)
splitStringLoop ([]) a = ([],a)
splitStringLoop (i:rest) a = if i /= ',' then splitStringLoop rest (a ++ [i]) else (rest, a)

readProgram :: IO ROM
readProgram = do
     line <- getLine
     let commands = splitString line
     return (map (\s -> read s :: Integer) commands)

readMemory :: ROM -> Integer -> Integer
readMemory r a = r !! (fromIntegral a)

writeMemory :: ROM -> Integer -> Integer -> ROM
writeMemory r a v = (take (fromIntegral a) r) ++ [v] ++ (drop ((fromIntegral a) + 1) r)

data CPU = CPU {memory :: ROM, ip :: Integer, finished :: Bool } deriving (Show)

step :: CPU -> CPU
step (CPU r i f)
    | f == True = CPU r i f
    | readMemory r i == 99 = CPU r i True
    | readMemory r i == 1 = cpuOperation (CPU r i f) (\a b->a+b)
    | readMemory r i == 2 = cpuOperation (CPU r i f) (\a b->a*b)
    | otherwise = CPU r i False

cpuOperation :: CPU -> (Integer -> Integer -> Integer) -> CPU
cpuOperation (CPU r i f) func = let
    first = readMemory r (readMemory r (i+1))
    second = readMemory r (readMemory r (i+2))
    result = func first second
    writeAddress = readMemory r (i+3)
    newRom = writeMemory r writeAddress result
    in CPU newRom (i+4) f

runProgram :: CPU -> [CPU]
runProgram c = let result = step c in
    if (finished result) == True then [result]
    else result:(runProgram result)

runProgramWithInputs :: CPU -> Integer -> Integer -> Integer
runProgramWithInputs c a b = let
    m = memory c
    newMemory = writeMemory (writeMemory m 1 a) 2 b
    result = runProgram $ CPU newMemory 0 False
    in
    head $ memory $ last result

testProgram :: CPU -> Integer -> Integer -> Bool
testProgram c a b = let out = runProgramWithInputs c a b
    in out == 19690720

all_params = [(a,b) | a <- [1..99], b<-[0..99]]

findGood (_,_,True) = True
findGood (_,_,False) = False

computeAnswer (a,b,_) = 100 * a + b

main = do
     rom <- readProgram
     let cpu = CPU rom 0 False
     let result = map (\(a,b) -> (a,b,(testProgram cpu a b))) all_params
     print $ computeAnswer $ head $ filter findGood result

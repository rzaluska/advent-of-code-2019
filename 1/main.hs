import System.IO (isEOF)

readLines:: IO [String]
readLines = do
  done <- isEOF
  if done then return ([])
  else do
    line <- getLine
    if null line then return ([])
    else do
        rest <- readLines
        return (line:rest)

calculate_fuel::Integer -> Integer
calculate_fuel mass = (mass `div` 3) - 2

convert_lines_to_int = map (\x -> read x :: Integer)
calculate_mass_for_whole_rocket_normal l = sum(map calculate_fuel l)

calculate_fuel_limit::Integer -> [Integer]
calculate_fuel_limit 0 = []
calculate_fuel_limit x =
    let curr = max (calculate_fuel x) 0
    in curr:(calculate_fuel_limit curr)

calculate_mass_for_whole_module l = sum $ calculate_fuel_limit l
calculate_mass_for_whole_rocket_new l = sum(map calculate_mass_for_whole_module l)

main = do
  lines <- readLines
  let ints = convert_lines_to_int lines
  putStrLn $ "1: " ++ (show(calculate_mass_for_whole_rocket_normal ints))
  putStrLn $ "2: " ++ (show(calculate_mass_for_whole_rocket_new ints))

module Day01 where

fuelCost :: Int -> Int
fuelCost m = (m `div` 3) - 2

fuelCostRecursive :: Int -> Int
fuelCostRecursive = sum . takeWhile (> 0) . tail . iterate fuelCost

day01 :: IO ()
day01 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2019/Inputs/day_01.txt"
  let input_ints = read <$> input_strs :: [Int]
      part1 = sum $ fuelCost <$> input_ints
      part2 = sum $ fuelCostRecursive <$> input_ints

  putStr "The solution to day 01 part 01 is "
  print part1
  putStr "The solution to day 01 part 02 is "
  print part2

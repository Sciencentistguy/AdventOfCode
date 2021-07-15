module Day01
  ( day01,
  )
where

import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

day01 :: IO ()
day01 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_01.txt"
  let input_ints = read <$> input_strs :: [Int]
  -- part 1
  putStr "The answer for day one part one is "
  print $ head do
    x <- input_ints
    y <- input_ints
    guard $ x + y == 2020
    return $ x * y
  -- part 2
  putStr "The answer for day one part two is "
  print $ head do
    x <- input_ints
    y <- input_ints
    z <- input_ints
    guard $ x + y + z == 2020
    return $ x * y * z

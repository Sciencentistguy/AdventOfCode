module Day04 where

import Common
import Data.List

monotonicIncrease :: Ord a => [a] -> Bool
monotonicIncrease s = sort s == s

sameGroups :: Eq a => [a] -> [Int]
sameGroups = map length . group

day04 :: IO ()
day04 = do
  input <- readFile "/home/jamie/Git/AdventOfCode/2019/Inputs/day_04.txt"
  let [begin, end] = read <$> split '-' input :: [Int]
  let nums = fmap sameGroups <$> filter monotonicIncrease $ show <$> [begin .. end]
  putStr "The solution to day 04 part 01 is "
  print $ count (any (> 1)) nums
  putStr "The solution to day 04 part 02 is "
  print $ count (elem 2) nums

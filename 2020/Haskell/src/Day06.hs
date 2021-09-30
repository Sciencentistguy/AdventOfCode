module Day06
  ( day06,
  )
where

import Common
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

day06 :: IO ()
day06 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_06.txt"
  let grouped_entries = groupEntries input_strs
  -- part 1
  putStr "The answer for day six part one is "
  print $ sum $ map (length . nub . filter (/= ' ')) grouped_entries
  -- part 2
  putStr "The answer for day six part two is "
  print $ sum $ map (length . foldl' intersect ['a' .. 'z'] . split ' ') grouped_entries

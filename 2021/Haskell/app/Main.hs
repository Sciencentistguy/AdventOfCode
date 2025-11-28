module Main where

import AoC
import Day01
import Day02
import Day03
import Day04
import Day05

main :: IO ()
main = do
  token <- getToken
  runAoC token day01
  runAoC token day02
  runAoC token day03
  runAoC token day04
  runAoC token day05

{-# LANGUAGE RankNTypes #-}

module Main where

import AoC
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day16
import Day17
import Safe
import System.Environment (getArgs)

main :: IO ()
main = do
  token <- getToken
  let runDay :: forall out a. Show out => Runner a out -> IO ()
      runDay = runAoC token

  (arg : _) <- getArgs

  let day = case readMay arg :: Maybe Int of
        Just x -> x
        Nothing -> error $ "Invalid (or no) day passed: " ++ arg

  case day of
    1 -> runDay day01
    2 -> runDay day02
    3 -> runDay day03
    4 -> runDay day04
    5 -> runDay day05
    6 -> runDay day06
    7 -> runDay day07
    8 -> runDay day08
    9 -> runDay day09
    10 -> runDay day10
    11 -> runDay day11
    12 -> runDay day12
    13 -> runDay day13
    14 -> runDay day14
    16 -> runDay day16
    17 -> runDay day17
    _ -> error $ "Day " ++ show day ++ " is not yet implemented."

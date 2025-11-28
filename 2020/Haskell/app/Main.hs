{-# LANGUAGE LambdaCase #-}

module Main where

import AoC
import Data.Text qualified as T
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06

-- import Day07
import Day08
import Day09
import Day10
import System.Environment (lookupEnv)

main :: IO ()
main = do
  token <-
    Token . T.pack . init
      <$> ( lookupEnv "TOKEN" >>= \case
              Nothing -> error "`TOKEN` environment variable must be set"
              Just a -> return a
          )
  runAoC token day01
  runAoC token day02
  runAoC token day03
  runAoC token day04
  runAoC token day05
  runAoC token day06
  -- runAoC token day07
  runAoC token day08
  runAoC token day09
  runAoC token day10

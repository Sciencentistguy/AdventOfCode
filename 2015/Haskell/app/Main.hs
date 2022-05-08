{-# LANGUAGE LambdaCase #-}

module Main where

import AOC
import qualified Data.Text as Text
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import System.Environment (lookupEnv)

main :: IO ()
main = do
  token <-
    Token . Text.pack . init
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
  runAoC token day07

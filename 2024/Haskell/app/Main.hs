{-# LANGUAGE RankNTypes #-}

module Main where

import AoC
import Day01
import Day02
import Day03
import Day05
import Day07
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
    5 -> runDay day05
    7 -> runDay day07
    _ -> error $ "Day " ++ show day ++ " is not yet implemented."

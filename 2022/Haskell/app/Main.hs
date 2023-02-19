module Main where

import AoC
import Day01
import Day02
import Safe
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let day = readMay $ head args :: Maybe Int
  token <- getToken
  case day of
    Nothing -> putStrLn "Please provide a day number as an argument."
    Just 1 -> runAoC token day01
    Just 2 -> runAoC token day02
    Just _ -> putStrLn "Day not implemented yet."

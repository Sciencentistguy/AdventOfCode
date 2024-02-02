module Main where

import AoC
import Day01
import Safe
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let day = case args of
        [x] -> readMay x :: Maybe Int
        _ -> Nothing
  token <- getToken
  case day of
    Nothing -> putStrLn "Please provide a day number as an argument."
    Just 1 -> runAoC token day01
    Just x -> do
      putStr "Day '" 
      putStr $ show x
      putStrLn "' not implemented yet."

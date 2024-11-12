module Main where

import AoC
import Day01
import Day02
import Day04
import Day05
import Day06
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
        Just 2 -> runAoC token day02
        Just 4 -> runAoC token day04
        Just 5 -> runAoC token day05
        Just 6 -> runAoC token day06
        Just x -> do
            putStr "Day '"
            putStr $ show x
            putStrLn "' not implemented yet."

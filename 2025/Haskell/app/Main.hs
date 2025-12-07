{-# LANGUAGE RankNTypes #-}

module Main where

import AoC
import Day01
import Safe
import System.Environment (getArgs)

main :: IO ()
main = do
  token <- getToken
  let runDay :: forall out a. (Show out) => Runner a out -> IO ()
      runDay = runAoC token

  (arg : _) <- getArgs

  let day = case readMay arg :: Maybe Int of
        Just x -> x
        Nothing -> error $ "Invalid (or no) day passed: " ++ arg

  case day of
    1 -> runDay day01
    _ -> error $ "Day " ++ show day ++ " is not yet implemented."

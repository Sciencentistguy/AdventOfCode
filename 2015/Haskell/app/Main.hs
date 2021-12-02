{-# LANGUAGE LambdaCase #-}

module Main where

import AOC
import qualified Data.Text as Text
import Day01
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

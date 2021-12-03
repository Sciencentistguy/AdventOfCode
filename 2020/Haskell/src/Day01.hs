module Day01
  ( day01,
  )
where

import AOC
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Safe (readMay)

type Parsed = [Int]

day01 :: Runner Parsed Int
day01 =
  let day = 01
      year = 2020
      parser = traverse (readMay . Text.unpack) . Text.lines
      part1 input = return $ head do
        x <- input
        y <- input
        guard $ x + y == 2020
        return $ x * y
      part2 input = return $ head do
        x <- input
        y <- input
        z <- input
        guard $ x + y + z == 2020
        return $ x * y * z
   in Runner {..}

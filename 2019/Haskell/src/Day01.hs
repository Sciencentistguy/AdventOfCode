module Day01 where

import AoC
import Data.Text (Text)
import qualified Data.Text as Text
import Safe

type Parsed = [Int]

fuelCost :: Int -> Int
fuelCost m = (m `div` 3) - 2

fuelCostRecursive :: Int -> Int
fuelCostRecursive = sum . takeWhile (> 0) . tail . iterate fuelCost

day01 :: Runner Parsed Int
day01 =
  let year = 2019
      day = 1
      parser :: Text -> Maybe Parsed
      parser input = traverse readMay (lines $ Text.unpack input)
      part1 = return . sum . fmap fuelCost
      part2 = return . sum . fmap fuelCostRecursive
   in Runner {..}

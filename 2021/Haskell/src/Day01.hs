module Day01 (
  day01,
)
where

import AoC
import Common (windows)
import Data.Text (Text)
import Data.Text qualified as Text
import Safe (readMay)

type Parsed = [Int]

data Direction = Increase | Decrease deriving (Show, Eq)

greaters :: (Ord a) => [a] -> [Direction]
greaters (a : cs@(b : _)) = greaters cs ++ [if a < b then Increase else Decrease]
greaters _ = []

day01 :: Runner Parsed Int
day01 =
  let year = 2021
      day = 1
      parser :: Text -> Maybe Parsed
      parser input = traverse (readMay . Text.unpack) (Text.lines input)
      part1 = return . length . filter (== Increase) . greaters
      part2 input =
        let sum3s = sum <$> windows 3 input
         in return $ length $ filter (== Increase) $ greaters sum3s
   in Runner{..}

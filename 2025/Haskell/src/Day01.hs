module Day01 (
  day01,
)
where

import AoC
import Data.Text (Text)
import Data.Text qualified as Text

type Parsed = ()
type Solution = Int

day01 :: Runner Parsed Solution
day01 =
  let year = 2024
      day = 01
      parser :: Text -> Maybe Parsed
      parser = undefined
      part1 :: Parsed -> Maybe Solution
      part1 = undefined
      part2 :: Parsed -> Maybe Solution
      part2 = undefined
   in Runner{..}

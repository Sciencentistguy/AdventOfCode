module DayXX (
  dayXX,
)
where

import AoC
import Data.Text (Text)
import Data.Text qualified as Text

type Parsed = ()
type Solution = Int

dayXX :: Runner Parsed Solution
dayXX =
  let year = 2025
      day = XX
      parser :: Text -> Maybe Parsed
      parser = undefined
      part1 :: Parsed -> Maybe Solution
      part1 = undefined
      part2 :: Parsed -> Maybe Solution
      part2 = undefined
   in Runner{..}

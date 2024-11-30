module Day01
  ( day01,
  )
where
import AoC
import Data.Text (Text)
import qualified Data.Text as Text

type Parsed = ()

day01 :: Runner Parsed Int
day01 =
  let year = 2024
      day = 1
      parser :: Text -> Maybe Parsed
      parser = undefined
      part1 = undefined
      part2 = undefined
   in Runner {..}

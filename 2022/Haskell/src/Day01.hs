module Day01 (day01) where

import AoC
import Common (split)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Safe (readMay)

type Parsed = [[Int]]

day01 :: Runner Parsed Int
day01 =
  let year = 2022
      day = 1
      parser :: Text -> Maybe Parsed
      parser = traverse (traverse readMay) . split "" . lines . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 = return . maximum . fmap sum
      part2 = return . sum . take 3 . reverse . sort . fmap sum
   in Runner {..}

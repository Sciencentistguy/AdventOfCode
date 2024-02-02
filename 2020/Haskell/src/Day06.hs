module Day06
  ( day06,
  )
where

import AoC
import Common
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Parsed = [String]

day06 :: Runner Parsed Int
day06 =
  let year = 2020
      day = 6
      parser = return . groupEntries . lines . Text.unpack
      part1 = return . sum . map (length . nub . filter (/= ' '))
      part2 = return . sum . map (length . foldl' intersect ['a' .. 'z'] . split ' ')
   in Runner {..}

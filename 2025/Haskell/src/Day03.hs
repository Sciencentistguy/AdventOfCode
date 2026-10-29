module Day03
  ( day03,
  )
where

import AoC
import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text

type Parsed = [Battery]

type Battery = String

type Solution = Int

initN :: Int -> [b] -> [b]
initN n ls = fst <$> zip ls (drop n ls)

calibrate :: Int -> Battery -> Int
calibrate size battery =
  go battery (size - 1) 0
  where
    go :: Battery -> Int -> Int -> Int
    go _ (-1) acc = acc
    go battery size acc =
      let max = maximum (initN size battery)
          maxIdx = fromJust $ elemIndex max battery
          battery' = drop (maxIdx + 1) battery
          max' = digitToInt max
       in go battery' (size - 1) (acc * 10 + max')

day03 :: Runner Parsed Solution
day03 =
  let year = 2025
      day = 03
      parser :: Text -> Maybe Parsed
      parser = return . fmap Text.unpack . Text.lines
      part1 :: Parsed -> Maybe Solution
      part1 = return . sum . fmap (calibrate 2)
      part2 :: Parsed -> Maybe Solution
      part2 = return . sum . fmap (calibrate 12)
   in Runner {..}

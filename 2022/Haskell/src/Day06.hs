module Day06 (day06) where

import AoC
import Common (windows)
import Data.Text (Text)
import qualified Data.Text as Text

type Parsed = String

allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = x `notElem` xs && allDifferent xs

detect :: Eq a => Int -> [a] -> Int
detect len packet =
  let w = zip [0 ..] $ windows len packet
   in len + fst (head $ filter (snd . fmap allDifferent) w)

day06 :: Runner Parsed Int
day06 =
  let year = 2022
      day = 6
      parser :: Text -> Maybe Parsed
      parser = return . Text.unpack . Text.strip
      part1 :: Parsed -> Maybe Int
      part1 = return . detect 4
      part2 = return . detect 14
   in Runner {..}
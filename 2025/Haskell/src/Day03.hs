module Day03
  ( day03,
  )
where

import AoC
import Data.Char (digitToInt)
import Data.Text (Text)
import Data.Text qualified as Text

type Parsed = [Battery]

type Battery = String

type Solution = Int

data MonotonicStack a = MonotonicStack
  { storage :: [a],
    limit :: Int
  }
  deriving (Show)

push :: (Ord a) => MonotonicStack a -> a -> MonotonicStack a
push (MonotonicStack {..}) x = case storage of
  [] -> MonotonicStack [x] limit
  (a : as) ->
    if a >= x
      then MonotonicStack (x : storage) limit
      else
        if limit > 0
          then push (MonotonicStack as (limit - 1)) x
          else MonotonicStack (x : storage) limit

joltage :: Int -> MonotonicStack Char -> Int
joltage len (MonotonicStack ls _) =
  foldl' (\acc x -> acc * 10 + digitToInt x) 0 $ take len (reverse ls)

empty :: Int -> MonotonicStack a
empty = MonotonicStack []

calibrate' :: Int -> Battery -> Int
calibrate' size battery =
  joltage size $ foldl' push (empty (length battery - size)) battery

day03 :: Runner Parsed Solution
day03 =
  let year = 2025
      day = 03
      parser :: Text -> Maybe Parsed
      parser = return . fmap Text.unpack . Text.lines
      part1 :: Parsed -> Maybe Solution
      part1 = return . sum . fmap (calibrate' 2)
      part2 :: Parsed -> Maybe Solution
      part2 = return . sum . fmap (calibrate' 12)
   in Runner {..}

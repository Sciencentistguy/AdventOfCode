{-# LANGUAGE OverloadedStrings #-}

module Day02 (
  day02,
)
where

import AoC
import AoC.Common (absdiff)
import Data.Text (Text)
import Data.Text qualified as Text
import Safe (readMay)

type Parsed = [[Int]]
type Solution = Int

increasing :: [Int] -> Bool
increasing xs = and $ zipWith (<=) xs (drop 1 xs)

decreasing :: [Int] -> Bool
decreasing xs = and $ zipWith (>=) xs (drop 1 xs)

monotonic :: [Int] -> Bool
monotonic = liftA2 (||) increasing decreasing

gradual :: [Int] -> Bool
gradual (a1 : a2 : xs) =
  let diff = absdiff a1 a2
   in if diff < 1 || diff > 3
        then False
        else gradual (a2 : xs)
gradual _ = True

safe :: [Int] -> Bool
safe = liftA2 (&&) gradual monotonic

tolerate :: [a] -> [[a]]
tolerate xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

day02 :: Runner Parsed Solution
day02 =
  let year = 2024
      day = 02
      parser :: Text -> Maybe Parsed
      parser = traverse (traverse (readMay . Text.unpack)) . fmap Text.words . Text.lines
      part1 :: Parsed -> Maybe Solution
      part1 = return . length . filter safe
      part2 :: Parsed -> Maybe Solution
      part2 = return . length . filter (any safe) . fmap tolerate
   in Runner{..}

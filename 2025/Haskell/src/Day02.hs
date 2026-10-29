{-# LANGUAGE OverloadedStrings #-}

module Day02
  ( day02,
  )
where

import AoC
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text

type Parsed = [Range]

type Range = (Int, Int)

type Solution = Int

unwrapDecimal :: Either a1 (a2, b) -> a2
unwrapDecimal x = case x of
  Right (a, _) -> a
  _ -> error "unwrap"

readInt :: Text -> Int
readInt = unwrapDecimal . Text.decimal

parse :: Text -> Parsed
parse input =
  let ranges = Text.split (== ',') input
      splitRanges = Text.split (== '-') <$> ranges
      parsedRanges = fmap readInt <$> splitRanges
      f [a, b] = (a, b)
      f _ = error "Too many elements in range"
   in f <$> parsedRanges

splat :: Range -> [Int]
splat (a, b) = [a .. b]

invalid2 :: Int -> Bool
invalid2 x =
  (x > 10 && x <= 99 && x `mod` 11 == 0)
    || (x > 1000 && x <= 9999 && x `mod` 101 == 0)
    || (x > 100000 && x <= 999999 && x `mod` 1001 == 0)
    || (x > 10000000 && x <= 99999999 && x `mod` 10001 == 0)
    || (x > 1000000000 && x <= 9999999999 && x `mod` 100001 == 0)

invalid3 :: Int -> Bool
invalid3 x =
  (x > 10 && x <= 99 && x `mod` 11 == 0)
    || (x > 100 && x <= 999 && x `mod` 111 == 0)
    || (x > 1000 && x <= 9999 && (x `mod` 101 == 0 || x `mod` 1111 == 0))
    || (x > 10000 && x <= 99999 && x `mod` 11111 == 0)
    || (x > 100000 && x <= 999999 && (x `mod` 1001 == 0 || x `mod` 10101 == 0 || x `mod` 111111 == 0))
    || (x > 1000000 && x <= 9999999 && x `mod` 1111111 == 0)
    || (x > 10000000 && x <= 99999999 && (x `mod` 10001 == 0 || x `mod` 1010101 == 0 || x `mod` 11111111 == 0))
    || (x > 100000000 && x <= 999999999 && (x `mod` 1001001 == 0 || x `mod` 111111111 == 0))
    || (x > 1000000000 && x <= 9999999999 && (x `mod` 100001 == 0 || x `mod` 101010101 == 0 || x `mod` 1111111111 == 0))

day02 :: Runner Parsed Solution
day02 =
  let year = 2025
      day = 02
      parser :: Text -> Maybe Parsed
      parser = return . parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . sum . filter invalid2 . (>>= splat)
      part2 :: Parsed -> Maybe Solution
      part2 = return . sum . filter invalid3 . (>>= splat)
   in Runner {..}

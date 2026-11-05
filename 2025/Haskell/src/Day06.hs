{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day06
  ( day06,
  )
where

import AoC
import AoC.Common
import Control.Lens
import Data.List (transpose)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)

data Parsed = Parsed
  { operators :: [[Int] -> Int],
    rows :: [[Int]],
    cols :: [[Int]]
  }

instance Show Parsed where
  show Parsed {..} = show cols

type Solution = Int

unwrapDecimal :: Either a1 (a2, b) -> a2
unwrapDecimal x = case x of
  Right (a, _) -> a
  _ -> error "unwrap"

windowRanges :: (Num a, Enum a, Show a) => [a] -> [[a]]
windowRanges xs = f <$> windows 2 xs
  where
    f [a, b] = [a .. (b - 2)]
    f x = unreachable $ show x

parse :: Text -> Parsed
parse input =
  let lines = Text.lines input
      (nums, ops) = fromJust $ unsnoc lines
      c = enumerate $ Text.unpack ops
      (operatorCols, operators) = unzip $ filter ((/= ' ') . snd) c
      operatorCols' = operatorCols ++ [Text.length ops + 1 {- + 1 for the lack of space column after the end  -}]
      rows = transpose $ fmap (unwrapDecimal . decimal :: Text -> Int) . Text.words <$> nums
      cols = transpose $ Text.unpack <$> nums
      cols' = fmap (read . (cols !!)) <$> windowRanges operatorCols'
      operators' =
        operators <&> \case
          '*' -> foldl' (*) 1
          '+' -> foldl' (+) 0
          c -> unreachable [c]
   in Parsed operators' rows cols'

solve :: (Parsed -> [[Int]]) -> Parsed -> Int
solve f x@Parsed {..} = sum $ zipWith ($) operators (f x)

day06 :: Runner Parsed Solution
day06 =
  let year = 2025
      day = 06
      parser :: Text -> Maybe Parsed
      parser = return . parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . solve rows
      part2 :: Parsed -> Maybe Solution
      part2 = return . solve cols
   in Runner {..}

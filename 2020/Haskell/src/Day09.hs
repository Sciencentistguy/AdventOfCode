{-# LANGUAGE BangPatterns #-}

module Day09 (day09) where

import AoC
import Common (windows)
import Control.Monad (guard)
import Data.List (scanl')
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Safe (readMay)

isSumOfTwo :: (Eq a, Num a) => a -> [a] -> Bool
isSumOfTwo v xs =
  or do
    x <- xs
    y <- xs
    guard $ x /= y
    let sum = x + y
    return $ v == sum

part1 :: (Eq a, Num a) => [a] -> a
part1 input =
  let x = windows 26 input
      y = (\xs -> (last xs, isSumOfTwo (last xs) (init xs))) <$> x
   in fst $ head $ dropWhile snd y

part2 :: (Num b, Ord b) => [b] -> b -> Maybe b
part2 input target = do
  let cumulative = V.fromList $ scanl' (+) 0 input
  (i, j) <- findBounds cumulative target
  let xs = take (j - i) $ drop i input
  return $ minimum xs + maximum xs
 where
  findBounds ns goal = go 0 1
   where
    go !i !j = do
      x <- ns V.!? i
      y <- ns V.!? j
      case compare (y - x) goal of
        LT -> go i (j + 1)
        EQ -> pure (i, j)
        GT -> go (i + 1) j

type Parsed = [Int]

day09 :: Runner Parsed Int
day09 =
  let year = 2020
      day = 9
      parser :: Text -> Maybe Parsed
      parser = traverse readMay . lines . Text.unpack
      part1 = return . Day09.part1
      part2 x =
        let p1 = Day09.part1 x
         in Day09.part2 x p1
   in Runner{..}

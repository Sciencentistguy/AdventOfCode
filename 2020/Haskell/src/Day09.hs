{-# LANGUAGE BangPatterns #-}

module Day09 (day09) where

import Common (windows)
import Control.Monad (guard)
import Control.Monad.Extra (anyM)
import Data.List (scanl')
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Vector.Fusion.Bundle (inplace)

isSumOfTwo :: (Eq a, Num a) => a -> [a] -> Bool
isSumOfTwo v xs =
  or do
    x <- xs
    y <- xs
    guard $ x /= y
    let sum = x + y
    return $ v == sum

part1 input =
  let x = windows 26 input
      y = (\xs -> (last xs, isSumOfTwo (last xs) (init xs))) <$> x
   in fst $ head $ dropWhile snd y

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

day09 :: IO ()
day09 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_09.txt"
  let input_ints = read <$> input_strs :: [Int]
  -- part 1
  putStr "The answer for day nine part one is "
  let p1 = part1 input_ints
  print p1
  -- part 2
  putStr "The answer for day nine part two is "
  print $ fromJust $ part2 input_ints p1

{-# LANGUAGE TupleSections #-}

module Day10 (day10) where

import Data.Foldable (Foldable (fold), find, toList)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe (fromJust)

freqs :: (Ord a, Hashable a) => [a] -> HM.HashMap a Int
freqs = HM.fromListWith (+) . fmap (,1) . toList

diffs :: Num a => [a] -> [a]
diffs xs@(_ : ys) = zipWith (-) ys xs
diffs [] = undefined

part1 xs = do
  let xs' = 0 : xs ++ [maximum xs + 3]
      stepFrequencies = freqs $ diffs $ sort xs'
  ones <- stepFrequencies HM.!? 1
  threes <- stepFrequencies HM.!? 3
  return $ ones * threes

pathsToGoal :: IS.IntSet -> IM.IntMap Integer
pathsToGoal is = res
  where
    res = flip IM.fromSet is $ \i ->
      if i == goal
        then 1
        else
          sum
            [ findOrZero (i + j) res
              | j <- [1, 2, 3]
            ]
    goal = IS.findMax is

toChain xs = xsset `IS.union` IS.fromList [0, top + 3]
  where
    xsset = IS.fromList xs
    top = IS.findMax xsset

findOrZero = IM.findWithDefault 0

day10 :: IO ()
day10 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_10.txt"
  let input_ints = read <$> input_strs :: [Int]
  -- part 1
  putStr "The answer for day ten part one is "
  print $ fromJust $ part1 input_ints
  -- part 2
  putStr "The answer for day ten part two is "
  let chain = toChain input_ints
  let paths = pathsToGoal chain
  print $ fromJust $ paths IM.!? 0

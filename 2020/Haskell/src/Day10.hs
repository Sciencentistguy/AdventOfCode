{-# LANGUAGE TupleSections #-}

module Day10 (day10) where

import AoC
import Data.Foldable (toList)
import Data.HashMap.Lazy qualified as HM
import Data.Hashable
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List
import Data.Text (Text)
import Data.Text qualified as Text
import Safe (readMay)

freqs :: (Ord a, Hashable a) => [a] -> HM.HashMap a Int
freqs = HM.fromListWith (+) . fmap (,1) . toList

diffs :: (Num a) => [a] -> [a]
diffs xs@(_ : ys) = zipWith (-) ys xs
diffs [] = undefined

part1 :: (Num k, Ord k, Hashable k) => [k] -> Maybe Int
part1 xs = do
  let xs' = 0 : xs ++ [maximum xs + 3]
      stepFrequencies = freqs $ diffs $ sort xs'
  ones <- stepFrequencies HM.!? 1
  threes <- stepFrequencies HM.!? 3
  return $ ones * threes

pathsToGoal :: IntSet -> IntMap Integer
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

toChain :: [IS.Key] -> IntSet
toChain xs = xsset `IS.union` IS.fromList [0, top + 3]
 where
  xsset = IS.fromList xs
  top = IS.findMax xsset

findOrZero :: IS.Key -> IntMap Integer -> Integer
findOrZero = IM.findWithDefault 0

type Parsed = [Int]

day10 :: Runner Parsed Integer
day10 =
  let year = 2020
      day = 10
      parser :: Text -> Maybe Parsed
      parser = traverse readMay . lines . Text.unpack
      -- part1 :: Parsed -> Maybe Integer
      part1 = fmap toInteger . Day10.part1
      -- part2 :: Parsed -> Maybe Integer
      part2 x =
        let chain = toChain x
            paths = pathsToGoal chain
         in paths IM.!? 0
   in Runner{..}

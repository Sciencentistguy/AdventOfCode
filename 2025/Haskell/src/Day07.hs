{-# LANGUAGE OverloadedStrings #-}

module Day07
  ( day07,
  )
where

import AoC
import AoC.Common (enumerate)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (elemIndex, elemIndices)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace

data Parsed = Parsed
  { start :: Int,
    splitters :: [[Int]]
  }

type Solution = Int

parse :: Text -> Parsed
parse input =
  let (start : splitters) = Text.unpack <$> Text.lines input
      start' = fromJust $ elemIndex 'S' start
      splitters' = elemIndices '^' <$> splitters
   in Parsed start' splitters'

p1 :: Parsed -> Solution
p1 Parsed {..} = snd (foldl' f (IntSet.singleton start, 0) splitters)
  where
    f (a, b) row =
      let relevant = filter (`IntSet.member` a) row
          without = a `IntSet.difference` IntSet.fromList relevant
          with = IntSet.fromList $ relevant >>= \a -> [a + 1, a - 1]
       in (without <> with, b + length relevant)

-- if IntSet.member i acc of
--   Just _ ->  undefined-- found
--   Nothing -> acc

-- if IntSet.member i acc
--    then
--    else y

-- testInput = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."

day07 :: Runner Parsed Solution
day07 =
  let year = 2025
      day = 07
      parser :: Text -> Maybe Parsed
      parser = return . parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . p1
      part2 :: Parsed -> Maybe Solution
      part2 = undefined
   in Runner {..}

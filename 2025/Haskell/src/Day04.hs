{-# LANGUAGE OverloadedStrings #-}

module Day04
  ( day04,
  )
where

import AoC
import AoC.Common
import Data.Array.IArray
import Data.Foldable
import Data.Functor ((<&>))
import Data.List ((\\))
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceShow, traceShowId)
import Linear.V2

type Parsed = Array (V2 Int) Slot

type Solution = Int

data Slot = Paper | Empty
  deriving (Show, Eq)

fromChar :: Char -> Slot
fromChar '@' = Paper
fromChar '.' = Empty
fromChar x = error $ "Unexpected char: " ++ [x]

parse :: Text -> Parsed
parse input =
  let lines = Text.unpack <$> Text.lines input
      width = length $ head lines
      height = length lines
      slots = lines >>= fmap fromChar
   in listArray (V2 0 0, V2 (height - 1) (width - 1)) slots

neighbours :: [V2 Int]
neighbours = range (V2 (-1) (-1), V2 1 1) \\ [V2 0 0]

removable :: Parsed -> [V2 Int]
removable grid =
  let x = assocs grid
      y = fst <$> filter ((== Paper) . snd) x
   in flip filter y $ \ix ->
        let a = (ix +) <$> neighbours
            b = mapMaybe (grid !?) a
            c = filter (== Paper) b
         in length c < 4

p1 :: Parsed -> Int
p1 = length . removable

remove :: Parsed -> Parsed
remove grid = grid // ((,Empty) <$> removable grid)

p2 :: Parsed -> Int
p2 grid =
  let grid' = fixp remove grid
      nbefore = countTrue (== Paper) grid
      nAfter = countTrue (== Paper) grid'
   in nbefore - nAfter

day04 :: Runner Parsed Solution
day04 =
  let year = 2025
      day = 04
      parser :: Text -> Maybe Parsed
      parser = return . parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . p1
      part2 :: Parsed -> Maybe Solution
      part2 = return . p2
   in Runner {..}

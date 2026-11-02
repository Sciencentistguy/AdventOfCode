{-# LANGUAGE OverloadedStrings #-}

module Day04
  ( day04,
  )
where

import AoC
import Data.Array.IArray
import Data.Functor ((<&>))
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceShow, traceShowId)
import Linear.V2
import Data.List ((\\))

type Parsed = Array (V2 Int) Slot

type Solution = Int

data Slot = Paper | Empty
  deriving (Show, Eq)

fromChar :: Char -> Slot
fromChar '@' = Paper
fromChar '.' = Empty
fromChar x = error $ "Unexpected char: " ++ [x]

parse input =
  let lines = Text.unpack <$> Text.lines input
      width = length $ head lines
      height = length lines
      slots = lines >>= fmap fromChar
   in listArray (V2 0 0, V2 (height - 1) (width - 1)) slots

-- countTrues = foldl' (\acc x -> if x then acc + 1 else acc) 0

cardinals = range (V2 (-1) (-1), V2 1 1)  \\ [V2 0 0]

p1 :: Parsed -> Int
p1 parsed =
  let x = assocs parsed
      y = filter ((== Paper) . snd) x
      z = flip filter y $ \(ix, _) ->
        let a = (ix +) <$> cardinals
            b = mapMaybe (parsed !?) a
            c = filter (== Paper) b
         in length c < 4
   in length z

-- testInput = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."

day04 :: Runner Parsed Solution
day04 =
  let year = 2025
      day = 04
      parser :: Text -> Maybe Parsed
      parser = return . parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . p1
      part2 :: Parsed -> Maybe Solution
      part2 = undefined
   in Runner {..}

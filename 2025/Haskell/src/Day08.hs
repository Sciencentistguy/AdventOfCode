{-# LANGUAGE OverloadedStrings #-}

module Day08
  ( day08,
  )
where

import AoC
import AoC.Common
-- (Metric (..), V3 (..))

import Control.Lens
import Data.Graph.Inductive
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Linear

type Parsed = [V3 Double]

type Solution = Int

parse :: Text -> Parsed
parse = fmap ((toV3 . fmap readDbl) . Text.splitOn ",") . Text.lines
  where
    readInt :: Text -> Int -- explicit type signature prevents defaulting to Integer
    readInt = unwrapDecimal . decimal
    readDbl = fromIntegral . readInt
    toV3 [a, b, c] = V3 a b c
    toV3 x = unreachable $ show x

distances :: (Ord b, Metric f, Floating b, Show (f b)) => [f b] -> [(f b, f b, b)]
distances positions = sortOn thrd $ f <$> choose 2 positions
  where
    f [a, b] = (a, b, distance a b)
    f x = unreachable $ show x

    thrd (_, _, x) = x

p1 :: Parsed -> Solution
p1 positions =
  let gr :: Gr (V3 Double) Double
      gr = fst $ mkMapGraph positions (take 1000 $ distances positions)
      comp = components gr
      comp' = sortOn Down $ length <$> comp
   in product $ take 3 comp'

p2 :: Parsed -> Solution
p2 positions =
  round $ go start $ distances positions
  where
    (start, nm) = mkMapGraph positions []
    go :: Gr (V3 Double) Double -> [(V3 Double, V3 Double, Double)] -> Double
    go _ [] = error "ran out of edges"
    go gr (x@(v1, v2, _) : xs) =
      let gr' = insMapEdge nm x gr
          numComponents = length $ components gr'
       in if numComponents == 1
            then v1 ^. _x * v2 ^. _x
            else go gr' xs

day08 :: Runner Parsed Solution
day08 =
  let year = 2025
      day = 08
      parser :: Text -> Maybe Parsed
      parser = return . parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . p1
      part2 :: Parsed -> Maybe Solution
      part2 = return . p2
   in Runner {..}

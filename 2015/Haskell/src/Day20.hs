module Day20
  ( day20,
  )
where

import AoC
import Data.List
import Data.Text (Text)
import Data.Text qualified as Text
import Math.NumberTheory.ArithmeticFunctions
import Safe

type Parsed = Int

presentsP1 :: Int -> Int
presentsP1 n = foldl' (\acc i -> acc + (10 * i)) 0 $ divisors n

presentsP2 :: Int -> Int
presentsP2 n =
  foldl'
    ( \acc i ->
        acc
          + if (n `div` i) <= 50
            then 11 * i
            else 0
    )
    0
    $ divisors n

day20 :: Runner Parsed Int
day20 =
  let year = 2015
      day = 20
      parser :: Text -> Maybe Parsed
      parser = readMay . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 target = (+ 1) <$> findIndex (>= target) (presentsP1 <$> [1 ..])
      part2 :: Parsed -> Maybe Int
      part2 target = (+ 1) <$> findIndex (>= target) (presentsP2 <$> [1 ..])
   in Runner {..}

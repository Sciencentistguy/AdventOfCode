module Day02
  ( day02,
  )
where

import AOC
import Common
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [(Int, Int, Int)]

pInput :: Parser (Int, Int, Int)
pInput = do
  l <- L.decimal
  _ <- char 'x'
  w <- L.decimal
  _ <- char 'x'
  h <- L.decimal
  return (l, w, h)

paperNeeded :: (Ord a, Num a) => (a, a, a) -> a
paperNeeded (l, w, h) =
  let areas = sort [l * w, w * h, h * l]
   in sum $ zipWith (*) [3, 2, 2] areas

ribbonNeeded :: (Num a, Ord a) => (a, a, a) -> a
ribbonNeeded (l, w, h) =
  let perimeters = sort [2 * (l + w), 2 * (w + h), 2 * (h + l)]
      volume = l * w * h
   in head perimeters + volume

day02 :: Runner Parsed Int
day02 =
  let year = 2015
      day = 2
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . traverse (parse pInput "input") . Text.lines
      part1 = return . sum . fmap paperNeeded
      part2 = return . sum . fmap ribbonNeeded
   in Runner {..}

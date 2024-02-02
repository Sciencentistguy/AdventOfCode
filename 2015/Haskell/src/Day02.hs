module Day02
  ( day02,
  )
where

import AoC
import Common
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Present = (Int, Int, Int)

type Parsed = [Present]

pInput :: Parser Present
pInput = do
  l <- L.decimal
  _ <- char 'x'
  w <- L.decimal
  _ <- char 'x'
  h <- L.decimal
  return (l, w, h)

paperNeeded :: Present -> Int
paperNeeded (l, w, h) =
  let areas = sort [l * w, w * h, h * l]
   in sum $ zipWith (*) [3, 2, 2] areas

ribbonNeeded :: Present -> Int
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

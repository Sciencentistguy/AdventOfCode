module Day04 (day04) where

import AoC
import Control.Applicative
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Common

type Parser = ParsecT Void Text Identity

data Range = Range
  { start :: Int,
    end :: Int
  }

contains :: Range -> Range -> Bool
contains (Range a b) (Range c d) = a <= c && b >= d

overlaps :: Range -> Range -> Bool
overlaps (Range a b) (Range c d) = a <= d && b >= c

pPair :: Parser (Range, Range)
pPair = do
  start <- pRange
  _ <- char ','
  end <- pRange
  return (start, end)

pRange :: Parser Range
pRange = do
  start <- L.decimal
  _ <- char '-'
  Range start <$> L.decimal

flippedEither :: (a -> a -> Bool) -> (a, a) -> Bool
flippedEither f = liftA2 (||) (uncurry f) (uncurry $ flip f)

type Parsed = [(Range, Range)]

day04 :: Runner Parsed Int
day04 =
  let year = 2022
      day = 4
      parser :: Text -> Maybe Parsed
      parser input = unwrapPEB $ traverse (parse pPair "<input>") $ Text.lines input
      part1 :: Parsed -> Maybe Int
      part1 = return . length . filter (flippedEither contains)
      part2 = return . length . filter (flippedEither overlaps)
   in Runner {..}
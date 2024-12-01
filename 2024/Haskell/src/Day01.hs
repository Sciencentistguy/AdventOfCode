module Day01
  ( day01,
  )
where

import AoC
import AoC.Common (Parser, parseLines, unwrapParser)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = ([Int], [Int])

sc :: Parser ()
sc = L.space space1 empty empty

pLine :: Parser (Int, Int)
pLine = do
  x <- L.decimal
  _ <- sc
  y <- L.decimal
  return (x, y)

absdiff :: Int -> Int -> Int
absdiff a b = abs (a - b)

sortBoth :: (Ord a1, Ord a2) => ([a1], [a2]) -> ([a1], [a2])
sortBoth (a, b) = (sort a, sort b)

countIn :: Int -> [Int] -> Int
countIn x = length . filter (== x)

day01 :: Runner Parsed Int
day01 =
  let year = 2024
      day = 1
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . fmap (sortBoth . unzip) . parseLines pLine
      part1 :: Parsed -> Maybe Int
      part1 = return . sum . uncurry (zipWith absdiff)
      part2 :: Parsed -> Maybe Int
      part2 (left, right) = return $ foldl (\acc x -> acc + x * countIn x right) 0 left
   in Runner {..}

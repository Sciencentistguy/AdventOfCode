module Day01 (
  day01,
)
where

import AoC
import AoC.Common (Parser, absdiff, parseLines, unwrapParser)
import Data.Bifunctor (Bifunctor (..))
import Data.List (sort)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parsed = ([Int], [Int])
type Solution = Int

sc :: Parser ()
sc = L.space space1 empty empty

pLine :: Parser (Int, Int)
pLine = do
  x <- L.decimal
  _ <- sc
  y <- L.decimal
  return (x, y)

sortBoth :: (Bifunctor f, Ord a, Ord b) => f [a] [b] -> f [a] [b]
sortBoth = bimap sort sort

countIn :: Int -> [Int] -> Int
countIn x = length . filter (== x)

day01 :: Runner Parsed Solution
day01 =
  let year = 2024
      day = 1
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . fmap (sortBoth . unzip) . parseLines pLine
      part1 :: Parsed -> Maybe Solution
      part1 = return . sum . uncurry (zipWith absdiff)
      part2 :: Parsed -> Maybe Solution
      part2 (left, right) = return $ foldl (\acc x -> acc + x * countIn x right) 0 left
   in Runner{..}

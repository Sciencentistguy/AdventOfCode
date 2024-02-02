{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day05
  ( day05,
  )
where

import AoC
import Common
import Control.Applicative (Applicative (liftA2))
import Data.Char (digitToInt)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Extra (intToFloat)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [Line]

type LineMap = HashMap (Int, Int) Int

data Line = Line
  { x1 :: Int,
    y1 :: Int,
    x2 :: Int,
    y2 :: Int
  }
  deriving (Show)

pLine :: Parser Line
pLine = do
  x1 <- L.decimal
  _ <- char ','
  y1 <- L.decimal
  _ <- string " -> "
  x2 <- L.decimal
  _ <- char ','
  y2 <- L.decimal
  return Line {..}

p1 :: LineMap -> [Line] -> LineMap
p1 map [] = map
p1 map (Line {..} : xs) =
  if not (x1 == x2 || y1 == y2)
    then p1 map xs
    else
      let points = pointsBetween (x1, y1) (x2, y2)
          x = HashMap.fromList $ (,1) <$> points
          map' = HashMap.unionWith (+) map x
       in p1 map' xs

p2 :: LineMap -> [Line] -> LineMap
p2 map [] = map
p2 map (Line {..} : xs) =
  let points = pointsBetween (x1, y1) (x2, y2)
      x = HashMap.fromList $ (,1) <$> points
      map' = HashMap.unionWith (+) map x
   in p2 map' xs

getNumberOfOverlaps :: HashMap a Int -> Int
getNumberOfOverlaps = length . filter ((> 1) . snd) . HashMap.toList

day05 :: Runner Parsed Int
day05 =
  let year = 2021
      day = 5
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . traverse (parse pLine "input") . Text.lines
      part1 = return . getNumberOfOverlaps . p1 mempty
      part2 = return . getNumberOfOverlaps . p2 mempty
   in Runner {..}

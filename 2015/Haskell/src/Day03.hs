module Day03
  ( day03,
  )
where

import AOC
import Common
import Data.Functor
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [Motion]

type Point = (Int, Int)

data Motion = North | South | East | West

data Mode = Robo | Santa deriving (Eq)

pMotion :: Parser Motion
pMotion =
  char '^' $> North
    <|> char 'v' $> South
    <|> char '>' $> East
    <|> char '<' $> West

pInput :: Parser Parsed
pInput = many pMotion

initialHS :: HashSet Point
initialHS = HS.insert (0, 0) HS.empty

advanceCoords :: (Num a1, Num a2) => Motion -> (a2, a1) -> (a2, a1)
advanceCoords direction (x, y) = case direction of
  North -> (x, y + 1)
  South -> (x, y -1)
  East -> (x + 1, y)
  West -> (x -1, y)

trackLocations :: [Motion] -> Int
trackLocations directions = go directions (0, 0) initialHS
  where
    go :: [Motion] -> Point -> HashSet Point -> Int
    go (a : as) p acc =
      let pNext = advanceCoords a p
          inserted = HS.insert pNext acc
       in go as pNext inserted
    go [] _ acc = length acc

getBinaryPosition :: Motion -> Point -> Point -> Mode -> (Point, Point)
getBinaryPosition move san rob mode =
  let advance = advanceCoords move
   in case mode of
        Santa -> (advance san, rob)
        Robo -> (san, advance rob)

updateBinaryHS :: (Eq a, Hashable a) => Mode -> a -> a -> HashSet a -> HashSet a
updateBinaryHS mode san rob acc = case mode of
  Santa -> HS.insert san acc
  Robo -> HS.insert rob acc

flipMode :: Mode -> Mode
flipMode x = case x of
  Robo -> Santa
  Santa -> Robo

trackBinaryLocations :: [Motion] -> Int
trackBinaryLocations directions = go directions (0, 0) (0, 0) initialHS Santa
  where
    go :: [Motion] -> Point -> Point -> HashSet Point -> Mode -> Int
    go (a : as) (xs, ys) (xr, yr) acc mode =
      let (s, r) = getBinaryPosition a (xs, ys) (xr, yr) mode
          inserted = updateBinaryHS mode s r acc
       in go as s r inserted (flipMode mode)
    go [] _ _ acc _ = length acc

day03 :: Runner Parsed Int
day03 =
  let year = 2015
      day = 3
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . parse pInput "input"
      part1 = return . trackLocations
      part2 = return . trackBinaryLocations
   in Runner {..}

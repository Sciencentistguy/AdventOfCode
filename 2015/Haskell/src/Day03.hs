module Day03
  ( day03,
  )
where

import AOC
import Common
import Data.Functor (($>))
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

data Mode = RoboSanta | Santa deriving (Eq)

pMotion :: Parser Motion
pMotion =
  char '^' $> North
    <|> char 'v' $> South
    <|> char '>' $> East
    <|> char '<' $> West

pInput :: Parser Parsed
pInput = many pMotion

initialHS :: HashSet Point
initialHS = HS.fromList [(0, 0)]

advanceCoords :: Motion -> Point -> Point
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

getPositions :: Motion -> Point -> Point -> Mode -> (Point, Point)
getPositions move san rob mode =
  let advance = advanceCoords move
   in case mode of
        Santa -> (advance san, rob)
        RoboSanta -> (san, advance rob)

other :: Mode -> Mode
other x = case x of
  RoboSanta -> Santa
  Santa -> RoboSanta

trackPart2Locations :: [Motion] -> Int
trackPart2Locations directions = go directions (0, 0) (0, 0) initialHS Santa
  where
    go :: [Motion] -> Point -> Point -> HashSet Point -> Mode -> Int
    go (motion : motions) santaPos roboSantaPos acc mode =
      let (santaPos', roboSantaPos') = getPositions motion santaPos roboSantaPos mode
          updated = case mode of
            Santa -> HS.insert santaPos' acc
            RoboSanta -> HS.insert roboSantaPos' acc
       in go motions santaPos' roboSantaPos' updated (other mode)
    go [] _ _ acc _ = length acc

day03 :: Runner Parsed Int
day03 =
  let year = 2015
      day = 3
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . parse pInput "input"
      part1 = return . trackLocations
      part2 = return . trackPart2Locations
   in Runner {..}

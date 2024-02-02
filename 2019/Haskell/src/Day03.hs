module Day03 where

import AoC
import Common
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = Map Coord Int

data Direction = Up | Down | Left | Right

data Instruction = Instruction Direction Int

pDirection :: Parser Direction
pDirection =
  asum
    [ char 'U' >> return Up,
      char 'D' >> return Down,
      char 'R' >> return Day03.Right,
      char 'L' >> return Day03.Left
    ]

pInstruction :: Parser Instruction
pInstruction =
  liftM2
    Instruction
    pDirection
    L.decimal

type Coord = (Int, Int)

nearestDistanceToOrigin :: M.Map Coord a -> Int
nearestDistanceToOrigin = minimum . map manhattan . M.keys

pathIntersections :: [[Instruction]] -> M.Map Coord Int
pathIntersections = foldl1' (M.intersectionWith (+)) . map distances

distances :: [Instruction] -> M.Map Coord Int
distances steps = M.fromListWith min (zip (generatePath steps) [1 ..])

generatePath :: [Instruction] -> [Coord]
generatePath =
  let f (Instruction d n) = replicate n $ toUnitVector d
   in scanl1 addCoord . (=<<) f

manhattan :: Num a => (a, a) -> a
manhattan (a, b) = abs a + abs b

addCoord :: Coord -> Coord -> Coord
addCoord (x, y) (dx, dy) = (x + dx, y + dy)

toUnitVector :: Direction -> Coord
toUnitVector Up = (0, -1)
toUnitVector Down = (0, 1)
toUnitVector Day03.Left = (-1, 0)
toUnitVector Day03.Right = (1, 0)

day03 :: Runner Parsed Int
day03 =
  let year = 2019
      day = 3
      parser :: Text -> Maybe Parsed
      parser input =
        return $
          pathIntersections $
            parseUnwrap
              . traverse (parse pInstruction "input")
              . Text.split (== ',')
              <$> Text.lines input
      part1 = return . nearestDistanceToOrigin
      part2 = return . minimum
   in Runner {..}

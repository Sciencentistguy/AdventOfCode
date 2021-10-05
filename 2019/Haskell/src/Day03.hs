module Day03 where

import Common
import Data.Bifunctor
import Data.Foldable
import Data.IntMap.Lazy (intersection)
import Data.List (foldl1', intersect)
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
   in scanl1 addCoord . concatMap f

manhattan (a, b) = abs a + abs b

addCoord :: Coord -> Coord -> Coord
addCoord (x, y) (dx, dy) = (x + dx, y + dy)

toUnitVector :: Direction -> Coord
toUnitVector Up = (0, -1)
toUnitVector Down = (0, 1)
toUnitVector Day03.Left = (-1, 0)
toUnitVector Day03.Right = (1, 0)

day03 :: IO ()
day03 = do
  wires@[wire1, wire2] <- do
    rawInput <- readFile "/home/jamie/Git/AdventOfCode/2019/Inputs/day_03.txt"
    let f = parse pInstruction "input"
    return $ parseUnwrap . traverse f . split ',' <$> lines rawInput

  let intersections = pathIntersections wires

  -- part 1
  putStr "The solution to day 03 part 01 is "
  print $ nearestDistanceToOrigin intersections
  -- part 2
  putStr "The solution to day 03 part 02 is "
  print $ minimum intersections

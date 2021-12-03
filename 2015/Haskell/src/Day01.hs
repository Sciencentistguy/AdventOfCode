module Day01
  ( day01,
  )
where

import AOC
import Data.Text (Text)
import qualified Data.Text as Text

type Parsed = [Direction]

data Direction = Up | Down deriving (Show, Eq)

pDirection :: Char -> Maybe Direction
pDirection c = case c of
  '(' -> return Up
  ')' -> return Down
  _ -> Nothing

sumFloors :: Num p => [Direction] -> p
sumFloors floors = go floors 0
  where
    go (x : xs) acc = case x of
      Up -> go xs (acc + 1)
      Down -> go xs (acc - 1)
    go _ acc = acc

trackFloors :: Int -> [Direction] -> Maybe Int
trackFloors target floors = go (zip [0 ..] floors) target 0
  where
    go ((i, x) : xs) target acc =
      if acc == target
        then Just i
        else case x of
          Up -> go xs target (acc + 1)
          Down -> go xs target (acc - 1)
    go [] _ _ = Nothing

day01 :: Runner Parsed Int
day01 =
  let year = 2015
      day = 1
      parser :: Text -> Maybe Parsed
      parser input = traverse pDirection (Text.unpack input)
      part1 = return . sumFloors
      part2 = trackFloors (-1)
   in Runner {..}

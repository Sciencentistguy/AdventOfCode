module Day01
  ( day01,
  )
where

import AOC
import Control.Applicative (Applicative (liftA2))
import Data.List (inits)
import Data.List.Extra (elemIndex)
import Data.Text (Text)
import qualified Data.Text as Text

type Parsed = [Char]

numUps :: [Char] -> Int
numUps = length . filter (== '(')

numDowns :: [Char] -> Int
numDowns = length . filter (== ')')

destinationFloor :: [Char] -> Int
destinationFloor = liftA2 (-) numUps numDowns

trackFloors' :: Int -> [Char] -> Maybe Int
trackFloors' target floors = elemIndex target $ destinationFloor <$> inits floors

day01 :: Runner Parsed Int
day01 =
  let year = 2015
      day = 1
      parser :: Text -> Maybe Parsed
      parser = return . Text.unpack
      part1 = return . destinationFloor
      part2 = trackFloors' (-1)
   in Runner {..}

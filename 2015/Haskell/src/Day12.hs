module Day12
  ( day12,
  )
where

import AOC
import Common
import Control.Applicative
import Data.Char (isDigit)
import Data.List (groupBy)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Safe

type Parsed = String

numbers :: String -> [Int]
numbers s = catMaybes $ readMay <$> groupBy f s
  where
    f a b = g a && g b
    g = liftA2 (||) (== '-') isDigit

day12 :: Runner Parsed Int
day12 =
  let year = 2015
      day = 12
      parser :: Text -> Maybe Parsed
      parser = return . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 inpt = return $ sum $ numbers inpt
      part2 = undefined
   in Runner {..}

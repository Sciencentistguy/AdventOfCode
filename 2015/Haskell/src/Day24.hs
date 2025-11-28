{-# LANGUAGE OverloadedStrings #-}

module Day24
  ( day24,
  )
where

import AoC
import AoC.Common (Parser, unwrapParser)
import Data.List.Extra (minimumOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec (parse)
import Text.Megaparsec.Char.Lexer qualified as L

type Parsed = [Int]

-- Get all combinations of given length
combinationsLen :: Int -> [a] -> [[a]]
combinationsLen 0 _ = [[]]
combinationsLen _ [] = []
combinationsLen n (x : xs) = map (x :) (combinationsLen (n - 1) xs) ++ combinationsLen n xs

group :: Int -> [Int] -> [Int]
group grps nums =
  let targetSum = sum nums `div` grps
      -- we know that the maximum length of a group is length/grps
      allCombs = (`combinationsLen` nums) =<< [2 .. (length nums `div` grps - 1)]
      -- only include groups that sum to targetSum
      validGroups = filter (\x -> sum x == targetSum) allCombs
   in minimumOn length validGroups

day24 :: Runner Parsed Int
day24 =
  let year = 2015
      day = 24
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . traverse (parse (L.decimal :: Parser Int) "(input)") . Text.lines
      part1 :: Parsed -> Maybe Int
      part1 = return . product . group 3
      part2 :: Parsed -> Maybe Int
      part2 = return . product . group 4
   in Runner {..}

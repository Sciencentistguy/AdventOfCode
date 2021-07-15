{-# LANGUAGE RecordWildCards #-}

module Day02
  ( day02,
  )
where

import Common
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data PasswordSpec = PasswordSpec
  { psFirstNum :: Int,
    psSecondNum :: Int,
    psChar :: Char,
    psString :: String
  }

pPassword :: Parser PasswordSpec
pPassword = do
  psFirstNum <- L.decimal
  _ <- char '-'
  psSecondNum <- L.decimal
  _ <- char ' '
  psChar <- letterChar
  _ <- char ':'
  _ <- char ' '
  psString <- some letterChar
  return PasswordSpec {..}

isValidPartOne :: PasswordSpec -> Bool
isValidPartOne PasswordSpec {..} =
  let count = countCharString psString psChar
   in count >= psFirstNum && count <= psSecondNum

isValidPartTwo :: PasswordSpec -> Bool
isValidPartTwo PasswordSpec {..} =
  let pos1 = psString !! (psFirstNum - 1)
      pos2 = psString !! (psSecondNum - 1)
   in (pos1 == psChar) /= (pos2 == psChar)

day02 :: IO ()
day02 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_02.txt"
  let parsed = case traverse (parse pPassword "input") input_strs of
        Right x -> x
        Left e -> error $ errorBundlePretty e
  -- part 1
  putStr "The answer for day two part one is "
  print $ length $ filter isValidPartOne parsed
  -- part 2
  putStr "The answer for day two part two is "
  print $ length $ filter isValidPartTwo parsed

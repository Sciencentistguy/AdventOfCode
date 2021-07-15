module Day04
  ( day04,
  )
where

import Common
import Data.Char
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Read (readMaybe)

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValidPassport :: [String] -> Bool
isValidPassport (key : value : _) = case key of
  "hgt" -> case unit of
    "cm" -> not $ height < 150 || height > 193
    "in" -> not $ height < 59 || height > 76
    _ -> False
    where
      unit = drop (length value - 2) value
      height = read $ take (length value - 2) value :: Int
  "byr" -> not $ valueAsInt < 1920 || valueAsInt > 2002
  "iyr" -> not $ valueAsInt < 2010 || valueAsInt > 2020
  "eyr" -> not $ valueAsInt < 2020 || valueAsInt > 2030
  "hcl" -> (length value == 7) && (head value == '#') && all isHexDigit (tail value)
  "ecl" -> case value of
    "amb" -> True
    "blu" -> True
    "brn" -> True
    "gry" -> True
    "grn" -> True
    "hzl" -> True
    "oth" -> True
    _ -> False
  "pid" -> (length value == 9) && all isDigit value
  "cid" -> True
  _ -> False
  where
    valueAsInt = read value
isValidPassport _ = unreachable ""

day04 :: IO ()
day04 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_04.txt"
  -- part 1
  let grouped_entries = groupEntries input_strs
      valid_passports = filter (\x -> all (`isInfixOf` x) requiredFields) grouped_entries
  putStr "The answer for day four part one is "
  print $ length valid_passports
  -- part 2
  let splitted = fmap (split ':') . split ' ' <$> valid_passports
  putStr "The answer for day four part two is "
  print $ length $ filter (all isValidPassport) splitted

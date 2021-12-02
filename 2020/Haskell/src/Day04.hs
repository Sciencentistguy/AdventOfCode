module Day04
  ( day04,
  )
where

import AOC
import Common
import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Read (readMaybe)

type Parsed = [String]

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

day04 =
  let day = 4
      year = 2020
      parser s =
        let input_strs = lines $ Text.unpack s
            grouped_entries = groupEntries input_strs
         in return $ filter (\x -> all (`isInfixOf` x) requiredFields) grouped_entries
      part1 = return . length
      part2 p =
        let splitted = fmap (split ':') . split ' ' <$> p
         in return $ length $ filter (all isValidPassport) splitted
   in Runner {..}

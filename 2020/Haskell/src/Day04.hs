module Day04
  ( day04,
  )
where

import Common
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Read (readMaybe)

requiredFields :: [Text.Text]
requiredFields = map Text.pack ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validatePassport :: [String] -> Bool
validatePassport (key : value : _) = case key of
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
  "hcl" ->
    (length value == 7) && (head value == '#')
      && all (`elem` "0123456789abcdef") (tail value)
  "ecl" -> value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  "pid" -> (length value == 9) && all (`elem` "0123456789") value
  "cid" -> True
  _ -> False
  where
    valueAsInt = read value
validatePassport _ = error "unreachable"

day04 :: IO ()
day04 = do
  input_Text <- Text.lines <$> Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_04.txt"
  let input_strs = map Text.unpack input_Text
  -- part 1
  let grouped_entries = groupEntries input_strs
  let valid_passports =
        filter
          (\passport -> all (\need -> Text.isInfixOf need (Text.pack passport)) requiredFields)
          grouped_entries
  putStr "The answer for day four part one is "
  print $ length valid_passports
  -- part 2
  putStr "The answer for day four part two is "
  let splitted = map (map (split ':') . split ' ') valid_passports
  print $ length $ filter (all validatePassport) splitted

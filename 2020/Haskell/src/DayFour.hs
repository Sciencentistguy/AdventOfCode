module DayFour
    ( dayFour
    )
where

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Common


requiredFields :: [Text.Text]
requiredFields = map Text.pack ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validatePassport :: [String] -> Bool
validatePassport ("hgt" : value : _) | unit == "cm" = not $ height < 150 || height > 193
                                     | unit == "in" = not $ height < 59 || height > 76
  where
    unit   = drop (length value - 2) value
    height = read $ take (length value - 2) value :: Int

validatePassport (key : value : _)
    | key == "byr" = not $ valueAsInt < 1920 || valueAsInt > 2002
    | key == "iyr" = not $ valueAsInt < 2010 || valueAsInt > 2020
    | key == "eyr" = not $ valueAsInt < 2020 || valueAsInt > 2030
    | key == "hcl" = (length value == 7) && (head value == '#') && all (`elem` "0123456789abcdef") (tail value)
    | key == "ecl" = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    | key == "pid" = (length value == 9) && all (`elem` "0123456789") value
    | key == "cid" = True
    | otherwise    = False
    where valueAsInt = read value :: Int


dayFour :: IO ()
dayFour = do
    input_Text <- Text.lines <$> Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_four.txt"
    let input_strs = map Text.unpack input_Text
    -- part 1
    let filtered = filter (\passport -> all (\need -> Text.isInfixOf need (Text.pack passport)) requiredFields)
                          (groupEntries input_strs)
    putStr "The answer for day four part one is "
    print $ length filtered
    -- part 2
    putStr "The answer for day four part two is "
    let splitted = map (map (split ':') . split ' ') filtered
    print $ length $ filter (all validatePassport) splitted

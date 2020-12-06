module DayTwo
    ( dayTwo
    )
where

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Common

type Line = (Int, Int, Char, String)

replaceHyphen :: Char -> Char
replaceHyphen '-' = ' '
replaceHyphen c   = c

parse :: [String] -> Line
parse (a : b : c : d : _) = (read a, read b, head c, d)

partOne :: Line -> Bool
partOne (firstNum, secondNum, char, string) = (count >= firstNum) && (count <= secondNum)
    where count = countCharString string char

partTwo :: Line -> Bool
partTwo (firstNum, secondNum, char, string) = (pos1 == char) /= (pos2 == char)
  where
    pos1 = string !! (firstNum - 1)
    pos2 = string !! (secondNum - 1)

dayTwo :: IO ()
dayTwo = do
    input_Text <- Text.lines <$> Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_two.txt"
    let input_strs     = map Text.unpack input_Text
    let removedHyphens = map (map replaceHyphen) input_strs
    let filteredColon  = map (filter (/= ':')) removedHyphens
    let split          = map words filteredColon
    let parsedInput    = map parse split
    -- part 1
    putStr "The answer for day two part one is "
    print $ length $ filter partOne parsedInput
    -- part 2
    putStr "The answer for day two part two is "
    print $ length $ filter partTwo parsedInput

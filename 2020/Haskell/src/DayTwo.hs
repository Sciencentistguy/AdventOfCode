module DayTwo
    ( dayTwo
    )
where

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

type Line = (Int, Int, Char, String)

replaceHyphen :: Char -> Char
replaceHyphen '-' = ' '
replaceHyphen c   = c

parse :: [String] -> Line
parse (a : b : c : d : _) = (read a, read b, head c, d)

countCharString :: String -> Char -> Int
countCharString str c = length $ filter (== c) str

partOne :: Line -> Bool
partOne (firstNum, secondNum, char, string) = (count >= firstNum) && (count <= secondNum)
    where count = countCharString string char

partTwo :: Line -> Bool
partTwo (firstNum, secondNum, char, string) = (pos1 == char) /= (pos2 == char)
  where
    pos1 = string !! (firstNum - 1)
    pos2 = string !! (secondNum - 1)

countTrues :: [Bool] -> Int
countTrues a = length $ filter id a


dayTwo :: IO ()
dayTwo = do
    input_Text <- fmap Text.lines (Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_two.txt")
    let input_strs     = map Text.unpack input_Text
    let removedHyphens = map (map replaceHyphen) input_strs
    let filteredColon  = map (filter (/= ':')) removedHyphens
    let split          = map words filteredColon
    let parsed         = map parse split
    -- part 1
    putStr "The answer for day two part one is "
    print $ countTrues $ map partOne parsed
    -- part 2
    putStr "The answer for day two part two is "
    print $ countTrues $ map partTwo parsed

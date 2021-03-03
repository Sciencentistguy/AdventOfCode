module Day05
    ( day05
    )
where

import           Data.Char                      ( digitToInt )
import qualified Data.List                     as List
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

type BoardingPass = (Int, Int)

repl :: Char -> Char
repl 'R' = '1'
repl 'L' = '0'
repl 'F' = '0'
repl 'B' = '1'
repl c   = c

readBinary :: String -> Int
readBinary = List.foldl' (\acc x -> acc * 2 + digitToInt x) 0

getId :: BoardingPass -> Int
getId (row, col) = col + (row * 8)

day05 :: IO ()
day05 = do
    input_Text <- Text.lines
        <$> Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_05.txt"
    let input_strs        = map Text.unpack input_Text
    let input_strs_binary = map (map repl) input_strs
    let rows_strs         = map (take 7) input_strs_binary
    let cols_strs         = map (drop 7) input_strs_binary
    let rows_ints         = map readBinary rows_strs
    let cols_ints         = map readBinary cols_strs
    let rows_cols         = zip rows_ints cols_ints
    -- part 1
    let ids               = map getId rows_cols

    putStr "The answer for day five part one is "
    print $ maximum ids
    -- part 2
    putStr "The answer for day five part two is "
    print
        $ head
        $ filter (\x -> (x - 1) `elem` ids && (x + 1) `elem` ids)
        $ filter (`notElem` ids) [minimum ids .. maximum ids]

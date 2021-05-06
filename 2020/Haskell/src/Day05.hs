module Day05
  ( day05,
  )
where

import Control.Monad
import Data.Char (digitToInt)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data BoardingPass = BoardingPass
  { row :: Int,
    col :: Int
  }

readBspToInt :: String -> Int
readBspToInt x = List.foldl' (\acc x -> acc * 2 + digitToInt x) 0 (bsp_to_binary_str <$> x)
  where
    bsp_to_binary_str 'R' = '1'
    bsp_to_binary_str 'L' = '0'
    bsp_to_binary_str 'F' = '0'
    bsp_to_binary_str 'B' = '1'
    bsp_to_binary_str c = c

getId :: BoardingPass -> Int
getId (BoardingPass row col) = col + (row * 8)

day05 :: IO ()
day05 = do
  input_Text <- Text.lines <$> Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_05.txt"
  let input_strs = map Text.unpack input_Text
  let rows_strs = map (take 7) input_strs
  let cols_strs = map (drop 7) input_strs
  let rows_ints = map readBspToInt rows_strs
  let cols_ints = map readBspToInt cols_strs
  let rows_cols = zipWith BoardingPass rows_ints cols_ints
  -- part 1
  let ids = map getId rows_cols
  putStr "The answer for day five part one is "
  print $ maximum ids
  -- part 2
  putStr "The answer for day five part two is "
  print $
    head $
      filter (liftM2 (&&) (neighboursInList ids) (`notElem` ids)) [minimum ids .. maximum ids]
  where
    neighboursInList ls x = elem (x -1) ls && elem (x + 1) ls

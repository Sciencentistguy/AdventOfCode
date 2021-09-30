module Day05
  ( day05,
  )
where

import Common
import Control.Monad
import Data.Bifunctor
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

neighboursInList :: (Foldable t, Eq a, Num a) => t a -> a -> Bool
neighboursInList ls x = elem (x -1) ls && elem (x + 1) ls

getId :: BoardingPass -> Int
getId (BoardingPass row col) = col + (row * 8)

day05 :: IO ()
day05 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_05.txt"
  let ids =
        getId
          <$> unzipWith
            BoardingPass
            (bimap readBspToInt readBspToInt . splitAt 7 <$> input_strs)
  -- part 1
  putStr "The answer for day five part one is "
  print $ maximum ids
  -- part 2
  putStr "The answer for day five part two is "
  print $
    head $
      filter
        ( liftM2
            (&&)
            (neighboursInList ids)
            (`notElem` ids)
        )
        [minimum ids .. maximum ids]

module Day03
  ( day03,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Map = [String]

isTree :: Map -> Int -> Int -> Bool
isTree map x y = ((map !! y) !! x_mod_length) == '#'
  where
    x_mod_length = x `mod` length (head map)

getTreesSlope :: Map -> Int -> Int -> Int
getTreesSlope worldMap xStep yStep = go worldMap xStep yStep 0 0 0
  where
    go worldMap xStep yStep x y acc
      | y >= yLimit = acc
      | isTree worldMap x y = nextStep (acc + 1)
      | otherwise = nextStep acc
      where
        nextStep = go worldMap xStep yStep (x + xStep) (y + yStep)
    yLimit = length worldMap

day03 :: IO ()
day03 = do
  input_Text <- Text.lines <$> Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_03.txt"
  let input_strs = map Text.unpack input_Text
  -- part 1
  putStr "The answer for day three part one is "
  print $ getTreesSlope input_strs 3 1
  -- part 2
  putStr "The answer for day three part two is "
  print $
    product $
      [getTreesSlope input_strs x 1 | x <- [1, 3 .. 7]]
        ++ [getTreesSlope input_strs 1 2]

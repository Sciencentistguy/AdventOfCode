module Day03
  ( day03,
  )
where

import AOC
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Parsed = [String]

type Map = [String]

isTree :: Map -> Int -> Int -> Bool
isTree map x y = ((map !! y) !! x_mod_length) == '#'
  where
    map_length = length $ head map
    x_mod_length = x `mod` map_length

getTreesSlope :: Map -> Int -> Int -> Int
getTreesSlope worldMap xStep yStep = go worldMap xStep yStep 0 0 0
  where
    yLimit = length worldMap
    go worldMap xStep yStep x y acc
      | y >= yLimit = acc
      | isTree worldMap x y = nextStep (acc + 1)
      | otherwise = nextStep acc
      where
        nextStep = go worldMap xStep yStep (x + xStep) (y + yStep)

day03 =
  let day = 3
      year = 2020
      parser x = return $ Text.unpack <$> Text.lines x
      part1 p = return $ getTreesSlope p 3 1
      part2 p =
        return $
          product $
            uncurry (getTreesSlope p)
              <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
   in Runner {..}

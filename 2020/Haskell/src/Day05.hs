module Day05
  ( day05,
  )
where

import AoC
import Common
import Control.Monad
import Data.Bifunctor
import Data.Char (digitToInt)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Parsed = [Int]

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
getId BoardingPass {..} = col + (row * 8)

day05 :: Runner Parsed Int
day05 =
  let year = 2020
      day = 5
      parser input =
        return $
          getId
            <$> unzipWith
              BoardingPass
              ( bimap
                  readBspToInt
                  readBspToInt
                  . splitAt 7
                  <$> lines (Text.unpack input)
              )
      part1 = return . maximum
      part2 ids =
        return $
          head $
            filter
              ( liftM2
                  (&&)
                  (neighboursInList ids)
                  (`notElem` ids)
              )
              [minimum ids .. maximum ids]
   in Runner {..}

{-# LANGUAGE OverloadedStrings #-}

module Day08
  ( day08,
  )
where

import AoC
import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as Text

type Parsed = [String]

lenString :: String -> Int
lenString = length

lenMemory :: String -> Int
lenMemory s = go s 0
  where
    go :: String -> Int -> Int
    go ['"'] acc = acc
    go ('"' : xs) acc = go xs acc
    go ('\\' : '\\' : xs) acc = go xs (acc + 1)
    go ('\\' : '"' : xs) acc = go xs (acc + 1)
    go ('\\' : 'x' : _ : _ : xs) acc = go xs (acc + 1)
    go (_ : xs) acc = go xs (acc + 1)
    go [] _ = error "shouldn't get here"

lenStringed :: String -> Int
lenStringed = length . show

diffTuple :: (Num a) => (a, a) -> a
diffTuple (a, b) = a - b

sumUnzipped :: ([Int], [Int]) -> (Int, Int)
sumUnzipped = bimap sum sum

day08 :: Runner Parsed Int
day08 =
  let year = 2015
      day = 8
      parser :: Text -> Maybe Parsed
      parser = return . lines . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 strings =
        return $
          diffTuple $
            sumUnzipped $
              unzip $
                (lenString &&& lenMemory) <$> strings
      part2 strings =
        return $
          diffTuple $
            sumUnzipped $
              unzip $
                (lenStringed &&& lenString) <$> strings
   in Runner {..}

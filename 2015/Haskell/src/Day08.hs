{-# LANGUAGE OverloadedStrings #-}

module Day08
  ( day08,
  )
where

import AOC
import Common
import Control.Arrow ((&&&))
import Control.Monad.ST
import Data.Array.ST
import Data.Bifunctor (Bifunctor, bimap)
import Data.Foldable
import Data.Functor (($>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import Debug.Trace
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

diffTuple :: Num a => (a, a) -> a
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

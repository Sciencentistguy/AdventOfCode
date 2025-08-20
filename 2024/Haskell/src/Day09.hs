{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day09
  ( day09,
  )
where

import AoC
import Control.Monad (forM_, guard, join)
import Control.Monad.Extra
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.List (findIndex, sortBy)
import Data.List.Extra (groupSortBy)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (MVector, Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Debug.Trace

type Parsed = Vector (Maybe Int)

type Solution = Int

fromChar :: Char -> Maybe Int
fromChar c
  | c >= '0' && c <= '9' = Just $ fromEnum c - fromEnum '0'
  | otherwise = Nothing

parse :: Text -> Maybe Parsed
parse input = do
  let mkIDs x = if (x .&. 1) == 0 then Just (x `div` 2) else Nothing
  let digits = filter (`elem` ['0' .. '9']) $ Text.unpack input
  numbers <- traverse fromChar digits
  let ids = mkIDs <$> [0 ..]
      x = zip ids numbers
  return $
    V.fromList $
      x >>= \(a, b) -> case a of
        Just a -> take b $ repeat $ Just a
        Nothing -> take b $ repeat Nothing

fileIds :: [Maybe Int] -> [(Int, Int)]
fileIds parsed =
  let x = zip parsed [0 ..]
      x' = filter (not . isNothing . fst) x
      x'' = fmap (\(a, b) -> (fromJust a, b)) x'
      sorted' = join $ (sortBy (comparing Down)) <$> groupSortBy (comparing snd) x''
   in reverse sorted'

ffs disk 1 = go disk 0
  where
    go disk i = do
      val <- MV.read disk i
      case val of
        Just _ -> go disk (i + 1)
        Nothing -> return i

-- find the first block of n Nothings

solve lR disk =
  let files = fileIds (V.toList disk)
      x = runST $ do
        disk <- V.thaw disk
        forM_ files $ \(length, src) -> do
          dest <- ffs disk (lR length)
          destVal <- MV.read disk dest
          when ((isNothing destVal) && (src > dest)) $ MV.swap disk src dest
        V.freeze disk
   in checksum $ traceShowId (V.toList x)

checksum :: [Maybe Int] -> Int
checksum = sum . catMaybes . zipWith (fmap . (*)) [0 ..]

testInput :: Text
testInput = "2333133121414131402"

day09 :: Runner Parsed Solution
day09 =
  let year = 2024
      day = 09
      parser :: Text -> Maybe Parsed
      parser = parse
      part1 :: Parsed -> Maybe Solution
      -- part1 _ = parse testInput >>= return . solve (const 1)
      part1 = return . solve (const 1)
      part2 :: Parsed -> Maybe Solution
      part2 = return . solve id
   in Runner {..}

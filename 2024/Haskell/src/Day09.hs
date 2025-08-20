{-# LANGUAGE OverloadedStrings #-}

module Day09
  ( day09,
  )
where

import AoC
import Data.Bits
import Data.List (findIndex, sortBy)
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.List.Extra (groupSortBy)
import Control.Monad (join)

type Parsed = [Maybe Int]

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
    x >>= \(a, b) -> case a of
      Just a -> take b $ repeat $ Just a
      Nothing -> take b $ repeat Nothing

fileIds :: Parsed -> [(Int, Int)]
fileIds parsed =
  let x = zip parsed [0..]
      x' = filter (not . isNothing . fst) x
      x'' = fmap (\(a, b) -> (fromJust a, b)) x'
      sorted' = join $ (sortBy (comparing Down)) <$> groupSortBy (comparing snd) x''
  in reverse sorted'

findFreeSpace :: [Maybe Int] -> Int -> Maybe Int
findFreeSpace disk 1 = findIndex (== Nothing) disk
findFreeSpace disk length = undefined

solve lengthResolver disk =
  let files = fileIds disk
      go' = foldl' (\acc (length, src) -> case findFreeSpace acc (lengthResolver length) of
            Just dest | isNothing $ disk !! dest -> swap src dest acc
            _-> acc
              ) 
      disk' = go' disk files
   in return $ checksum $ disk'

swap :: Int -> Int -> [a] -> [a]
swap x y xs
  | x == y = error "Cannot swap the same index"
  | otherwise = [select i e | (i, e) <- zip [0 ..] xs]
  where
    ex = xs !! x
    ey = xs !! y
    select i e
      | i == x = ey
      | i == y = ex
      | otherwise = e

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
      part1 = {-parse testInput >>=-} solve (const 1)
      part2 :: Parsed -> Maybe Solution
      part2 = undefined
   in Runner {..}

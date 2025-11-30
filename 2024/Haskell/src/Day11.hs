{-# LANGUAGE OverloadedStrings #-}

module Day11 (
  day11,
)
where

import AoC
import AoC.Common
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Debug.Trace (traceShow)

type Parsed = HashMap Integer Int
type Solution = Int

splitDigits :: Integer -> [Integer]
splitDigits 0 = [0]
splitDigits x = reverse $ go x
  where
    go 0 = []
    go n = let (q, r) = n `divMod` 10 in
      r : go q

joinDigits :: [Integer] -> Integer
joinDigits = foldl (\acc d -> acc * 10 + d) 0



-- fn split_middle(num: u128) -> Option<(u128, u128)> {
    -- // Count number of digits
    -- let mut temp = num;
    -- let mut digit_count = 0;
    -- while temp > 0 {
        -- digit_count += 1;
        -- temp /= 10;
    -- }
    -- 
    -- if digit_count % 2 != 0 {
        -- return None;
    -- }
    -- 
    -- let divisor = 10_u128.pow(digit_count / 2 );
-- 
    -- let second = num % divisor;
    -- let first = num / divisor;
    -- 
    -- Some((first, second))
-- }

splitMiddle :: Integer -> Maybe (Integer, Integer)
splitMiddle n =
  let digits = splitDigits (fromIntegral n)
      len = length digits
   in if odd len
        then Nothing
        else
          let (firstDigits, secondDigits) = splitAt (len `div` 2) digits
              first = joinDigits firstDigits
              second = joinDigits secondDigits
           in Just (first, second)

processStones :: HashMap Integer Int -> HashMap Integer Int
processStones stones = HashMap.foldlWithKey' processStone HashMap.empty stones 
  where
    processStone acc stone count =
      case stone of 
        0 -> HashMap.insertWith (+) 0 (count * 2) acc
        _ -> case splitMiddle stone of
          Nothing -> HashMap.insertWith (+) stone count acc
          Just (first, second) ->
            let acc1 = HashMap.insertWith (+) first count acc
                acc2 = HashMap.insertWith (+) second count acc1
            in acc2
  

f2 = foldl' (\acc n -> HashMap.insertWith (+) n 1 acc) HashMap.empty

day11 :: Runner Parsed Solution
day11 =
  let year = 2024
      day = 11
      parser :: Text -> Maybe Parsed
      parser = fmap f2 . traverse (ok . fmap fst . decimal) . Text.splitOn " "
      part1 :: Parsed -> Maybe Solution
      part1 input = return . length $ (!! 25) $ iterate processStones input
      part2 :: Parsed -> Maybe Solution
      part2 input = return . length $ (!! 75) $ iterate processStones input
   in Runner{..}

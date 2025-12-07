module Day02 (
  day02,
)
where

import AoC
import Data.Text (Text)
import Data.Text qualified as Text

type Parsed = [(Int, Int)]
type Solution = Int

isRepeated2 :: Int -> Bool
isRepeated2 n
        | (n >= 10 && n < 99) && n `mod` 11 == 0 = true 
        | (n >= 1000 && n < 9999) && n `mod` 101 == 0 = true 
        | (n >= 100000 && n < 999999) && n `mod` 1001 == 0 = true 
        | (n >= 10000000 && n < 99999999) && n `mod` 10001 == 0 = true 
        | (n >= 1000000000 && n < 9999999999) && n `mod` 100001 == 0 = true 
        | otherwise = false
    
parse input = split

day02 :: Runner Parsed Solution
day02 =
  let year = 2025
      day = 02
      parser :: Text -> Maybe Parsed
      parser = undefined
      part1 :: Parsed -> Maybe Solution
      part1 = undefined
      part2 :: Parsed -> Maybe Solution
      part2 = undefined
   in Runner{..}

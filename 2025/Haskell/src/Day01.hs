module Day01
  ( day01,
  )
where

import AoC
import Data.Text (Text)
import Data.Text qualified as Text

type Parsed = [Int]

type Solution = Int

p :: [Char] -> Int
p ('L' : num) = negate $ read num
p ('R' : num) = read num
p _ = error "no"

p1 :: [Int] -> Solution
p1 = go 50 0
  where
    go _ count [] = count
    go dial count (x : xs) =
      let dial' = dial + x
       in case dial' `mod` 100 of
            0 -> go dial' (count + 1) xs
            _ -> go dial' count xs

-- A Mess :tm:
divEuclid :: (Integral a) => a -> a -> a
divEuclid a b =
  let q = a `div` b
   in if a `mod` b < 0
        then
          ( if b > 0
              then q - 1
              else q + 1
          )
        else q

p2 :: [Int] -> Solution
p2 = go 50 0
  where
    go _ count [] = count
    go dial count (rotation : xs) =
      let start = dial
          end = dial + rotation
          count' =
            count
              + if rotation > 0
                then
                  end `divEuclid` 100 - start `divEuclid` 100
                else
                  if rotation < 0
                    then
                      (start - 1) `divEuclid` 100 - (end - 1) `divEuclid` 100
                    else 0
       in go end count' xs

day01 :: Runner Parsed Solution
day01 =
  let year = 2025
      day = 01
      parser :: Text -> Maybe Parsed
      parser = return . fmap (p . Text.unpack) . Text.lines
      part1 :: Parsed -> Maybe Solution
      part1 = return . p1
      part2 :: Parsed -> Maybe Solution
      part2 = return . p2
   in Runner {..}

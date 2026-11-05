{-# LANGUAGE OverloadedStrings #-}

module Day05
  ( day05,
  )
where

import AoC
import AoC.Common
import Control.Lens
import Data.List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)

type Parsed = ([Int], [Range])

type Range = (Int, Int)

type Solution = Int

unwrapDecimal :: Either a1 (a2, b) -> a2
unwrapDecimal x = case x of
  Right (a, _) -> a
  _ -> error "unwrap"

listToTuple :: MonadFail m => [b] -> m (b, b)
listToTuple x = do
    [a, b] <- return x
    return (a, b)

parse' :: MonadFail m => Text -> m Parsed
parse' input = do 
  let lines = Text.lines input 
  [ranges, ingredients] <- return $ split mempty lines
  let ranges' = Text.splitOn "-" <$> ranges
      ranges'' = fmap (unwrapDecimal . decimal) <$> ranges'
      ingredients' = fmap (unwrapDecimal . decimal) ingredients
  ranges''' <- traverse listToTuple  ranges''
  return (ingredients', ranges''')

inRange :: Int -> Range -> Bool
inRange x (min, max) = x >= min && x <= max

p1 :: [Int] -> [Range] -> Int
p1 ingredients freshness = length $ filter (\x -> any (x `inRange`) freshness) ingredients

merge :: [Range] -> [Range]
merge xs = go xs []
  where
    go [] acc = acc
    go (x : xs) [] = go xs [x]
    go (incoming : xs) (prev : accs) = go xs
      if prev ^. _2 >= incoming ^. _1
        then (prev ^. _1, max (incoming ^. _2) (prev ^. _2)) : accs
        else incoming : prev : accs

p2 :: [Range] -> Int
p2 freshness =
  let x = sortOn fst freshness
      merged = merge x
   in sum $ (+ 1) . uncurry (flip (-)) <$> merged

day05 :: Runner Parsed Solution
day05 =
  let year = 2025
      day = 05
      parser :: Text -> Maybe Parsed
      parser = parse'
      part1 :: Parsed -> Maybe Solution
      part1 = return . uncurry p1
      part2 :: Parsed -> Maybe Solution
      part2 = return . p2 . snd
   in Runner {..}

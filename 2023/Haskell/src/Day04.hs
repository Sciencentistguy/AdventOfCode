module Day04 (day04) where

import AoC
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

type Parsed = [Card]

data Card = Card
    { winningNumbers :: [Int]
    , numbers :: [Int]
    }

pCard :: Text -> Maybe Card
pCard line = do
    let line' = Text.concat $ tail $ Text.split (== ':') line
    [winningNumbers, numbers] <-
        traverse (traverse (readMaybe . Text.unpack) . Text.words) $ Text.split (== '|') line'
    return Card{..}

matches :: Card -> [Int]
matches Card{..} = filter (`elem` winningNumbers) numbers

day04 :: Runner Parsed Int
day04 =
    let year = 2023
        day = 4
        parser :: Text -> Maybe Parsed
        parser = traverse pCard . Text.lines
        part1 :: Parsed -> Maybe Int
        part1 =
            let value [] = 0
                value x = 2 ^ (length x - 1)
             in return . sum . fmap (value . matches)
        part2 :: Parsed -> Maybe Int
        part2 =
            let
                f :: (Int, Card) -> Map Int Int -> Map Int Int
                f (n, c) acc = M.insert n value acc
                  where
                    numMatches = length $ matches c
                    value = succ $ sum $ take numMatches $ M.elems acc
             in
                return . sum . M.elems . foldr f mempty . zip [1 ..]
     in Runner{..}

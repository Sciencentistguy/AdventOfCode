module Day04 where

import AOC
import Common
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Safe (readMay)

type Parsed = [[Int]]

monotonicIncrease :: Ord a => [a] -> Bool
monotonicIncrease s = sort s == s

sameGroups :: Eq a => [a] -> [Int]
sameGroups = map length . group

day04 :: Runner Parsed Int
day04 =
  let year = 2019
      day = 4
      parser :: Text -> Maybe Parsed
      parser input = do
        [begin, end] <- traverse readMay $ split '-' $ Text.unpack input :: Maybe [Int]
        return $ fmap sameGroups <$> filter monotonicIncrease $ show <$> [begin .. end]
      part1 = return . count (any (> 1))
      part2 = return . count (elem 2)
   in Runner {..}

module Day07
  ( day07,
  )
where

import AoC
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (elemIndex, elemIndices, scanl')
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text

data Parsed = Parsed
  { start :: Int,
    splitters :: [[Int]]
  }

type Solution = Int

parse :: Text -> Maybe Parsed
parse input = do
  (start : splitters) <- return $ Text.unpack <$> Text.lines input
  let start' = fromJust $ elemIndex 'S' start
      splitters' = elemIndices '^' <$> splitters
  return $ Parsed start' splitters'

beams :: Parsed -> [IntMap Int]
beams Parsed {..} = scanl' f (IntMap.singleton start 1) splitters
  where
    f acc row =
      let relevant = IntMap.filterWithKey (\k _ -> k `elem` row) acc
          without = acc `IntMap.difference` relevant
          with = IntMap.fromListWith (+) $ IntMap.toList relevant >>= \(i, x) -> [(i - 1, x), (i + 1, x)]
       in IntMap.unionWith (+) without with

-- For p1 we need to count the number of times the beam encounters a splitter
p1 :: Parsed -> Solution
p1 parsed@Parsed {..} =
  let states = beams parsed
   in sum $ zipWith f states splitters
  where
    f state row = length $ IntMap.restrictKeys state (IntSet.fromList row)

day07 :: Runner Parsed Solution
day07 =
  let year = 2025
      day = 07
      parser :: Text -> Maybe Parsed
      parser = parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . p1
      part2 :: Parsed -> Maybe Solution
      part2 = return . sum . last . beams
   in Runner {..}

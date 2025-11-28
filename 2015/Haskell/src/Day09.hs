module Day09
  ( day09,
  )
where

import AoC
import AoC.Common
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (permutations)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Safe

type Distances = HashMap (String, String) Int

type Parsed = ([String], Distances)

pRoute :: (Read a) => [String] -> Maybe (String, String, a)
pRoute [origin, "to", destination, "=", distance] = do
  distance <- readMay distance
  return (origin, destination, distance)
pRoute _ = error "bad input"

pInput :: String -> Maybe Parsed
pInput inpt =
  let routes = traverse (pRoute . words) $ lines inpt
      cities = HS.toList . foldl' insertCities mempty <$> routes
      distances = foldl' insertBothWays mempty <$> routes
   in liftA2 (,) cities distances
  where
    insertBothWays distances (origin, destination, distance) =
      HM.insert (origin, destination) distance $
        HM.insert (destination, origin) distance distances
    insertCities cities (origin, destination, _) =
      HS.insert origin $
        HS.insert destination cities

dist :: Distances -> [String] -> Maybe Int
dist distances route = sum <$> traverse distance (tiles 2 1 route)
  where
    distance [origin, destination] = HM.lookup (origin, destination) distances
    distance _ = error "bad legDist"

type CmpBy a = (a -> a -> Ordering) -> [a] -> a

soln :: CmpBy [String] -> ([String], Distances) -> Maybe Int
soln best (cities, distances) =
  let optRoute = best (comparing (dist distances)) $ permutations cities
   in dist distances optRoute

day09 :: Runner Parsed Int
day09 =
  let year = 2015
      day = 9
      parser :: Text -> Maybe Parsed
      parser = pInput . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 = soln minimumBy
      part2 = soln maximumBy
   in Runner {..}

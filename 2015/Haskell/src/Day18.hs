{-# LANGUAGE OverloadedStrings #-}

module Day18
  ( day18,
  )
where

import AoC
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data State = On | Off deriving (Show, Eq)

toState :: Char -> State
toState = \case
  '.' -> Off
  '#' -> On
  _ -> error "aa"

toggle :: State -> State
toggle On = Off
toggle Off = On

type Parsed = Map (Int, Int) State

{- ORMOLU_DISABLE -}
neighbhours :: (Num a, Num b) => (a, b) -> [(a, b)]
neighbhours (i, j) =
  [ (i - 1, j - 1), (i, j - 1), (i + 1, j - 1),
    (i - 1, j    ),             (i + 1, j    ),
    (i - 1, j + 1), (i, j + 1), (i + 1, j + 1)
  ]
{- ORMOLU_ENABLE -}

updateLightAt :: Parsed -> Parsed -> (Int, Int) -> Parsed
updateLightAt state unchanged loc =
  let me = state Map.! loc
      numOnNeighbours = length $ filter (== On) $ mapMaybe (state Map.!?) $ neighbhours loc
      toggled = Map.adjust toggle loc unchanged
   in case (me, numOnNeighbours) of
        (On, 2) -> unchanged
        (On, 3) -> unchanged
        (On, _) -> toggled
        (Off, 3) -> toggled
        (Off, _) -> unchanged

updateAllLights :: Part -> Parsed -> Parsed
updateAllLights part lights =
  case part of
    Part1 -> foldl' (updateLightAt lights) lights grid
    Part2 ->
      let brokens = [(0, 0), (0, size), (size, 0), (size, size)]
          f acc loc = if loc `elem` brokens then acc else updateLightAt lights acc loc
       in foldl' f lights grid
  where
    size = maximum $ snd <$> Map.keys lights
    grid = [(i, j) | i <- [0 .. size], j <- [0 .. size]]

runForN :: Int -> (Parsed -> Parsed) -> Parsed -> Parsed
runForN n f lights = go lights 0
  where
    go lights n' = if n' == n then lights else go (f lights) (n' + 1)

countOn :: Parsed -> Int
countOn map = length $ filter (== On) $ snd <$> Map.toList map

pGrid :: Text -> Parsed
pGrid inpt =
  let chars = Text.unpack <$> Text.lines inpt
      enumerated = zip [0 :: Int ..] $ zip [0 :: Int ..] <$> chars
   in flatten enumerated
  where
    flatten x = go x mempty
    go ((i, x) : xs) acc = go xs $ go2 i x acc
    go [] acc = acc
    go2 _ [] acc = acc
    go2 i ((j, c) : xs) acc = go2 i xs (Map.insert (i, j) (toState c) acc)

day18 :: Runner Parsed Int
day18 =
  let year = 2015
      day = 18
      parser :: Text -> Maybe Parsed
      parser = return . pGrid
      part1 :: Parsed -> Maybe Int
      part1 = return . countOn . runForN 100 (updateAllLights Part1)
      part2 = return . countOn . runForN 100 (updateAllLights Part2)
   in Runner {..}

module Day14
  ( day14,
  )
where

import AoC
import AoC.Common (modifyAtN, runNTimes)
import Data.List.Extra (maximumOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Safe

type Parsed = [Reindeer]

data Reindeer = Reindeer
  { speed :: Int,
    flyTime :: Int,
    restTime :: Int
  }
  deriving (Show)

pLine :: Text -> Maybe Reindeer
pLine text = do
  [_, _, _, speed', _, _, flyTime', _, _, _, _, _, _, restTime', _] <- return $ Text.unpack <$> Text.split (== ' ') text
  speed <- readMay speed'
  flyTime <- readMay flyTime'
  restTime <- readMay restTime'
  return Reindeer {..}

data Mode = Flying | Resting deriving (Show)

data State = State
  { deer :: Reindeer,
    distance :: Int,
    stateTime :: Int,
    state :: Mode,
    points :: Int
  }
  deriving (Show)

mkInitialState :: Reindeer -> State
mkInitialState deer = State deer 0 0 Flying 0

step :: State -> State
step st@State {..} = case state of
  Flying ->
    if stateTime >= flyTime deer
      then st {stateTime = 1, state = Resting}
      else st {distance = distance + speed deer, stateTime = stateTime + 1}
  Resting ->
    if stateTime >= restTime deer
      then st {stateTime = 1, distance = distance + speed deer, state = Flying}
      else st {stateTime = stateTime + 1}

stepAll :: [State] -> [State]
stepAll states =
  let allStepped = step <$> states
      inLeadIdx = fst $ maximumOn (distance . snd) $ zip [0 :: Int ..] allStepped
   in modifyAtN (\st@State {..} -> st {points = points + 1}) inLeadIdx allStepped

race :: [Reindeer] -> [State]
race deers =
  let startingStates = mkInitialState <$> deers
   in runNTimes 2503 stepAll startingStates

day14 :: Runner Parsed Int
day14 =
  let year = 2015
      day = 14
      parser :: Text -> Maybe Parsed
      parser = traverse pLine . Text.lines
      part1 = return . distance . maximumOn distance . race
      part2 = return . points . maximumOn points . race
   in Runner {..}

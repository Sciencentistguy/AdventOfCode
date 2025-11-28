{-# LANGUAGE TemplateHaskell #-}

module Day14
  ( day14,
  )
where

import AoC
import AoC.Common (runNTimes)
import Control.Lens
import Data.List.Extra (maximumOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Safe

type Parsed = [Reindeer]

data Reindeer = Reindeer
  { _speed :: Int,
    _flyTime :: Int,
    _restTime :: Int
  }
  deriving (Show)

pLine :: Text -> Maybe Reindeer
pLine text = do
  [_, _, _, speed', _, _, flyTime', _, _, _, _, _, _, restTime', _] <- return $ Text.unpack <$> Text.split (== ' ') text
  _speed <- readMay speed'
  _flyTime <- readMay flyTime'
  _restTime <- readMay restTime'
  return Reindeer {..}

data Mode = Flying | Resting deriving (Show)

toggleMode :: Mode -> Mode
toggleMode Flying = Resting
toggleMode Resting = Flying

data State = State
  { _deer :: Reindeer,
    _distance :: Int,
    _stateTime :: Int,
    _state :: Mode,
    _points :: Int
  }
  deriving (Show)

makeLenses ''Reindeer
makeLenses ''State

mkInitialState :: Reindeer -> State
mkInitialState deer = State deer 0 0 Flying 0

step :: State -> State
step st = case st ^. state of
  Flying ->
    if st ^. stateTime >= st ^. deer . flyTime
      then st & flipState . resetStateTime
      else st & flyForward . wait1
  Resting ->
    if st ^. stateTime >= st ^. deer . restTime
      then st & flipState . flyForward . resetStateTime
      else st & wait1
  where
    wait1 = stateTime %~ (+ 1)
    flyForward st = st & distance %~ (+ (st ^. deer . speed))
    flipState = state %~ toggleMode
    resetStateTime = stateTime .~ 1

stepAll :: [State] -> [State]
stepAll states =
  let allStepped = step <$> states
      maxDistance = fromJust $ maximumOf (traversed . distance) allStepped
   in allStepped & traversed . filtered (\s -> s ^. distance == maxDistance) . points %~ (+ 1)

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
      part1 = return . (^. distance) . maximumOn (^. distance) . race
      part2 = return . (^. points) . maximumOn (^. points) . race
   in Runner {..}

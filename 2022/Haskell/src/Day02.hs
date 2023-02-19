module Day02 (day02) where

import AoC
import Data.Text (Text)
import qualified Data.Text as Text

data Play = Rock | Paper | Scissors deriving (Show, Eq)

data Suggestion = X | Y | Z deriving (Show, Eq)

data Game = Game
  { opponent :: Play,
    suggestion :: Suggestion
  }
  deriving (Show, Eq)

data Outcome = Win | Lose | Draw deriving (Show, Eq)

type Parsed = [Game]

pPlay :: Char -> Maybe Play
pPlay x = case x of
  'A' -> Just Rock
  'B' -> Just Paper
  'C' -> Just Scissors
  _ -> Nothing

pSuggestion :: Char -> Maybe Suggestion
pSuggestion x = case x of
  'X' -> Just X
  'Y' -> Just Y
  'Z' -> Just Z
  _ -> Nothing

pPair :: [Char] -> Maybe Game
pPair [a, b] = case (pPlay a, pSuggestion b) of
  (Just opponent, Just suggestion) -> return Game {..}
  _ -> Nothing
pPair _ = Nothing

matchScore :: Play -> Play -> Int
matchScore a b = case outcomeOf a b of
  Win -> 6
  Draw -> 3
  Lose -> 0
  where
    beats :: Play -> Play -> Bool
    a `beats` b = case (a, b) of
      (Rock, Scissors) -> True
      (Paper, Rock) -> True
      (Scissors, Paper) -> True
      _ -> False

    outcomeOf :: Play -> Play -> Outcome
    outcomeOf a b
      | a == b = Draw
      | a `beats` b = Win
      | otherwise = Lose

playScore :: Play -> Int
playScore a = case a of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

part1' :: Monad m => Game -> m Int
part1' Game {..} = do
  let toPlay = case suggestion of
        X -> Rock
        Y -> Paper
        Z -> Scissors
  return $ playScore toPlay + matchScore toPlay opponent

part2' :: Monad m => Game -> m Int
part2' Game {..} = do
  let toPlay = case suggestion of
        X -> willLose opponent
        Y -> willDraw opponent
        Z -> willBeat opponent
  return $ playScore toPlay + matchScore toPlay opponent
  where
    willBeat :: Play -> Play
    willBeat a = case a of
      Rock -> Paper
      Paper -> Scissors
      Scissors -> Rock

    willDraw :: Play -> Play
    willDraw = id

    willLose :: Play -> Play
    willLose a = case a of
      Rock -> Scissors
      Paper -> Rock
      Scissors -> Paper

day02 :: Runner Parsed Int
day02 =
  let year = 2022
      day = 2
      parser :: Text -> Maybe Parsed
      parser input = traverse pPair $ fmap head . words <$> lines (Text.unpack input)
      part1 :: Parsed -> Maybe Int
      part1 = fmap sum . traverse part1'
      part2 = fmap sum . traverse part2'
   in Runner {..}
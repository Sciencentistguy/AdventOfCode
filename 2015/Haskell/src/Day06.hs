{-# LANGUAGE OverloadedStrings #-}

module Day06
  ( day06,
  )
where

import AOC
import Common
import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Data.Functor (($>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [Instruction]

data Operation = TurnOn | TurnOff | Toggle

data Range = Range (Int, Int) (Int, Int)

data Instruction = Instruction Operation Range

pOperation :: Parser Operation
pOperation = (string "turn on" $> TurnOn) <|> (string "turn off" $> TurnOff) <|> (string "toggle" $> Toggle)

pPoint :: Parser (Int, Int)
pPoint = do
  first <- L.decimal
  _ <- char ','
  second <- L.decimal
  return (first, second)

pRange :: Parser Range
pRange = do
  first <- pPoint
  _ <- string " through "
  Range first <$> pPoint

pInstruction :: Parser Instruction
pInstruction = do
  op <- pOperation
  _ <- char ' '
  Instruction op <$> pRange

bound :: Int
bound = 999

allCoords :: [(Int, Int)]
allCoords = [(x, y) | x <- [0 .. bound], y <- [0 .. bound]]

type LightArray s a = (STUArray s) (Int, Int) a

operateLights :: MArray a t f => a (Int, Int) t -> (t -> t) -> Range -> f ()
operateLights lights op range = traverse_ modifyWithOp $ eachCoordWithin range
  where
    eachCoordWithin ((Range (x1, y1) (x2, y2))) =
      [ (x, y)
        | x <- [x1 `min` x2 .. x1 `max` x2],
          y <- [y1 `min` y2 .. y1 `max` y2]
      ]
    modifyWithOp coord = do
      b <- readArray lights coord
      writeArray lights coord (op b)

type Interpretation a = Operation -> a -> a

english :: Interpretation Bool
english TurnOff = const False
english TurnOn = const True
english Toggle = not

elvish :: Interpretation Int
elvish TurnOff x = 0 `max` (x - 1)
elvish TurnOn x = x + 1
elvish Toggle x = x + 2

runCommand :: MArray a t f => a (Int, Int) t -> Interpretation t -> Instruction -> f ()
runCommand lights interp (Instruction op range) = operateLights lights (interp op) range

initLights :: MArray (STUArray s) a (ST s) => a -> ST s (LightArray s a)
initLights = newArray ((0, 0), (bound, bound))

solve ::
  MArray (STUArray s) a (ST s) =>
  [Instruction] ->
  a ->
  Interpretation a ->
  ([a] -> Int) ->
  ST s Int
solve inpt initialValue interp summarize = do
  lights <- initLights initialValue
  traverse_ (runCommand lights interp) inpt
  ts <- mapM (readArray lights) allCoords
  return $ summarize ts

day06 :: Runner Parsed Int
day06 =
  let year = 2015
      day = 6
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . traverse (parse pInstruction "input") . Text.lines
      part1 inpt = return $ runST $ solve inpt False english (length . filter id)
      part2 inpt = return $ runST $ solve inpt 0 elvish sum
   in Runner {..}

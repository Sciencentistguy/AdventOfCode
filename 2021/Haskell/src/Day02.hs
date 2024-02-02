{-# LANGUAGE OverloadedStrings #-}

module Day02
  ( day02,
  )
where

import AoC
import Common
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [Instruction]

data Direction = Forward | Down | Up deriving (Show)

data Instruction = Instruction
  { direction :: Direction,
    amount :: Int
  }
  deriving (Show)

pInstruction :: Parser Instruction
pInstruction = do
  direction <- pDirection
  _ <- char ' '
  amount <- L.decimal
  return Instruction {..}

pDirection :: Parser Direction
pDirection =
  string "up" $> Up
    <|> string "down" $> Down
    <|> string "forward" $> Forward

moveAbs :: [Instruction] -> (Int, Int)
moveAbs instrs = go instrs 0 0
  where
    go (a : xs) depth horiz = case a of
      Instruction Forward x -> go xs depth (horiz + x)
      Instruction Down x -> go xs (depth + x) horiz
      Instruction Up x -> go xs (depth - x) horiz
    go [] depth horiz = (depth, horiz)

moveAim :: [Instruction] -> (Int, Int)
moveAim instrs = go instrs 0 0 0
  where
    go (a : xs) aim depth horiz = case a of
      Instruction Forward x -> go xs aim (depth + (aim * x)) (horiz + x)
      Instruction Down x -> go xs (aim + x) depth horiz
      Instruction Up x -> go xs (aim - x) depth horiz
    go [] _ depth horiz = (depth, horiz)

day02 :: Runner Parsed Int
day02 =
  let year = 2021
      day = 2
      parser :: Text -> Maybe Parsed
      parser input = unwrapParser $ traverse (parse pInstruction "input") (Text.lines input)
      part1 = return . uncurry (*) . moveAbs
      part2 = return . uncurry (*) . moveAim
   in Runner {..}

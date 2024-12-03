{-# LANGUAGE OverloadedStrings #-}

module Day03 (
  day03,
)
where

import AoC
import AoC.Common (Parser, absdiff, parseLines, unwrapParser)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceShow, traceShowId)
import Safe (readMay)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug
import Text.Regex.TDFA

data Instruction = Mul Int Int | Do | Dont
  deriving (Show)

type Parsed = [Instruction]

type Solution = Int

execute :: Instruction -> Int
execute (Mul x y) = x * y
execute _ = 0

executeIf :: Bool -> Instruction -> Int
executeIf True = execute
executeIf False = const 0

regex :: Text
regex = "mul\\(([0-9]+),([0-9]+)\\)|(don't\\(\\))|(do\\(\\))"

pInputRegex :: Text -> Maybe [Instruction]
pInputRegex text =
  let matches = text =~ regex :: [[Text]]
      parseMatch [_, x, y, "", ""] = Just $ Mul (read $ Text.unpack x) (read $ Text.unpack y)
      parseMatch [_, _, _, "don't()", _] = Just Dont
      parseMatch [_, _, _, _, "do()"] = Just Do
      parseMatch x = traceShow x Nothing
   in return $ mapMaybe parseMatch matches

day03 :: Runner Parsed Solution
day03 =
  let year = 2024
      day = 03
      parser :: Text -> Maybe Parsed
      parser = pInputRegex
      part1 :: Parsed -> Maybe Solution
      part1 = return . foldl' (\acc instr -> acc + execute instr) 0
      part2 :: Parsed -> Maybe Solution
      part2 = return . go True 0
       where
        go _ acc [] = acc
        go enabled acc (instr : instrs) =
          case instr of
            Mul _ _ -> go enabled (acc + executeIf enabled instr) instrs
            Do -> go True acc instrs
            Dont -> go False acc instrs
   in Runner{..}

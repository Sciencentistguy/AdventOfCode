{-# LANGUAGE OverloadedStrings #-}

module Day05 (day05) where

import AoC
import Common (unwrapPEB)
import Control.Lens
import Data.Char (isDigit)
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

data Instruction = Instruction
  { number :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

newtype Crates = Crates [[Char]] deriving (Show)

type Parsed = (Crates, [Instruction])

pInput :: Parser Parsed
pInput = do
  crates <- pCrates
  instructions <- catMaybes <$> sepBy (optional pInstruction) (char '\n')
  return (crates, instructions)

pInstruction :: Parser Instruction
pInstruction = do
  _ <- string "move "
  number <- L.decimal
  _ <- string " from "
  from <- pred <$> L.decimal -- 1-indexed
  _ <- string " to "
  to <- pred <$> L.decimal -- 1-indexed
  return Instruction {..}

indexAll :: Show a => [Int] -> [a] -> [a]
indexAll indices ls = (ls !!) <$> indices

pCrates :: Parser Crates
pCrates = do
  input <- lines <$> manyTill anySingle (string "\n\n")
  let numberRow = last input
      crateLines = init input
      num_stacks = length $ filter isDigit numberRow
      points = [0 .. (num_stacks - 1)] <&> (\n -> 4 * n + 1)
      x :: [[Char]]
      x = indexAll points <$> crateLines
      x' = transpose x
      x'' = reverse <$> x'
      x''' = filter (/= ' ') <$> x''
  return $ Crates x'''

common :: ([Char] -> [Char]) -> Crates -> Instruction -> Crates
common f (Crates crates) Instruction {..} =
  let mid = length (crates !! from) - number
      (leave, take) = splitAt mid $ crates !! from
      crates' = element from .~ leave $ crates
      crates'' = element to .~ (crates !! to ++ f take) $ crates'
   in Crates crates''

solve :: ([Char] -> [Char]) -> Crates -> [Instruction] -> [Char]
solve solver crates instrs = go crates instrs
  where
    go (Crates crates) [] = last <$> crates
    go crates (instr : xs) = go (common solver crates instr) xs

day05 :: Runner Parsed String
day05 =
  let year = 2022
      day = 5
      parser :: Text -> Maybe Parsed
      parser input = unwrapPEB $ parse pInput "<input>" input
      part1 :: Parsed -> Maybe String
      part1 = return <$> uncurry (solve reverse)
      part2 = return <$> uncurry (solve id)
   in Runner {..}
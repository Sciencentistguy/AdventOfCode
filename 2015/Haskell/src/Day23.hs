{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day23
  ( day23,
  )
where

import AoC
import AoC.Common (Parser, unwrapParser)
import Control.Applicative ((<|>))
import Control.Lens
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = Vector Instruction

type Offset = Int

data Register = A | B deriving (Show)

data Instruction = Hlf Register | Tpl Register | Inc Register | Jmp Offset | Jie Register Offset | Jio Register Offset deriving (Show)

pInstr :: Parser Instruction
pInstr =
  do
    constructor <- pComplexJmp
    _ <- space1
    reg <- pRegister
    _ <- char ','
    _ <- space1
    constructor reg <$> pOffset
    <|> do
      constructor <- pJmp
      _ <- space1
      constructor <$> pOffset
    <|> do
      constructor <- pRegisterOpcode
      _ <- space1
      constructor <$> pRegister

pRegisterOpcode :: Parser (Register -> Instruction)
pRegisterOpcode =
  string "hlf" $> Hlf
    <|> string "tpl" $> Tpl
    <|> string "inc" $> Inc

pJmp :: Parser (Offset -> Instruction)
pJmp = string "jmp" $> Jmp

pComplexJmp :: Parser (Register -> Offset -> Instruction)
pComplexJmp = string "jie" $> Jie <|> string "jio" $> Jio

pRegister :: Parser Register
pRegister =
  char 'a' $> A
    <|> char 'b' $> B

pOffset :: Parser Offset
pOffset = do
  sign <- pSign
  (* sign) <$> L.decimal

pSign :: Parser Int
pSign =
  char '+' $> 1
    <|> char '-' $> (-1)

data VM = VM
  { _a :: Int,
    _b :: Int,
    _pc :: Int
  }
  deriving (Show)

makeLenses ''VM

f :: VM -> Instruction -> VM
f vm instruction = case instruction of
  Hlf r -> vm & pc %~ (+ 1) & r %%~ (`div` 2)
  Tpl r -> vm & pc %~ (+ 1) & r %%~ (* 3)
  Inc r -> vm & pc %~ (+ 1) & r %%~ (+ 1)
  Jmp off -> vm & (pc %~ (+ off))
  Jie r off ->
    if even $ vm ^^. r
      then vm & pc %~ (+ off)
      else vm & pc %~ (+ 1)
  Jio r off ->
    if vm ^^. r == 1
      then vm & pc %~ (+ off)
      else vm & pc %~ (+ 1)
  where
    reg A = a
    reg B = b
    a ^^. b = a ^. reg b
    a %%~ b = reg a %~ b

runVM :: VM -> Parsed -> VM
runVM vm instructions = case instructions Vector.!? (vm ^. pc) of
  Nothing -> vm
  Just inst -> runVM (f vm inst) instructions

day23 :: Runner Parsed Int
day23 =
  let year = 2015
      day = 23
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . traverse (parse pInstr "(input)") . Vector.fromList . Text.lines
      part1 :: Parsed -> Maybe Int
      part1 = return . (^. b) . runVM (VM 0 0 0)
      part2 :: Parsed -> Maybe Int
      part2 = return . (^. b) . runVM (VM 1 0 0)
   in Runner {..}

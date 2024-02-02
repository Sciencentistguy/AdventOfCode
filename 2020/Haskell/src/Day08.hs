{-# LANGUAGE OverloadedStrings #-}

module Day08
  ( day08,
  )
where

import AoC
import Common
import Control.Comonad.Store (ComonadStore (peeks))
import Control.Lens (holesOf)
import Control.Lens.Tuple (_1)
import Control.Monad.Primitive (PrimMonad (PrimState), RealWorld)
import Control.Monad.ST
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Instruction = Nop Int | Acc Int | Jmp Int
  deriving (Show)

pInstr :: Parser Instruction
pInstr = do
  opcode <-
    (string "nop" >> return Nop)
      <|> (string "acc" >> return Acc)
      <|> (string "jmp" >> return Jmp)
  _ <- char ' '
  sign <- char '+' <|> char '-'
  number <- L.decimal
  case sign of
    '+' -> return $ opcode number
    '-' -> return $ opcode (negate number)
    _ -> undefined

runInstr :: Int -> Int -> Instruction -> (Int, Int)
runInstr pc acc instr = case instr of
  Nop _ -> (pc + 1, acc)
  Acc op -> (pc + 1, acc + op)
  Jmp op -> (pc + op, acc)

part1 :: PrimMonad m => MVector (PrimState m) (Instruction, Bool) -> m Int
part1 trackedInstructions = go 0 0
  where
    go pc acc = do
      (instr, ranBefore) <- MV.read trackedInstructions pc
      if ranBefore
        then return acc
        else do
          MV.write trackedInstructions pc (instr, True)
          let (pc', acc') = runInstr pc acc instr
          go pc' acc'

part2 :: PrimMonad m => Vector (Instruction, Bool) -> m Int
part2 instructions = do
  let programs = peeks flipInstr <$> holesOf (traverse . _1) instructions
  programs <- traverse V.thaw programs
  ranPrograms <- traverse (go 0 0) programs
  return $ head $ catMaybes ranPrograms
  where
    go pc acc trackedInstructions = do
      --(instr, ranBefore) <- trackedInstructions `readMay` pc
      instrpair <- trackedInstructions `readMay` pc
      case instrpair of
        -- past the end, program terminated
        Nothing -> return $ Just acc
        Just (instr, ranBefore) -> do
          if ranBefore
            then return Nothing
            else do
              MV.write trackedInstructions pc (instr, True)
              let (pc', acc') = runInstr pc acc instr
              go pc' acc' trackedInstructions

readMay :: PrimMonad m => MVector (PrimState m) a -> Int -> m (Maybe a)
readMay vec idx =
  if idx >= MV.length vec
    then return Nothing
    else Just <$> MV.read vec idx

flipInstr :: Instruction -> Instruction
flipInstr instr = case instr of
  Nop a -> Jmp a
  Acc a -> Acc a
  Jmp a -> Nop a

type Parsed = Vector (Instruction, Bool)

day08 :: Runner Parsed Int
day08 =
  let year = 2020
      day = 8
      parser input = do
        instructions <-
          fmap V.fromList $
            unwrapParser $
              traverse (parse pInstr "input") (Text.lines input)
        return $ V.zip instructions (V.replicate (V.length instructions) False)
      part1 x = return $ runST $ V.thaw x >>= Day08.part1
      part2 x = return $ runST $ Day08.part2 x
   in Runner {..}

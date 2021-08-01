module Day08
  ( day08,
  )
where

import Common
import Control.Comonad.Store (ComonadStore (peeks))
import Control.Lens (holesOf)
import Control.Lens.Tuple (_1)
import Control.Monad.Primitive (PrimMonad (PrimState), RealWorld)
import Control.Monad.ST (RealWorld)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
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

part1 :: MV.MVector RealWorld (Instruction, Bool) -> IO Int
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

part2 :: V.Vector (Instruction, Bool) -> IO Int
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

readMay :: PrimMonad m => MV.MVector (PrimState m) a -> Int -> m (Maybe a)
readMay vec idx =
  if idx >= MV.length vec
    then return Nothing
    else Just <$> MV.read vec idx

flipInstr instr = case instr of
  Nop a -> Jmp a
  Acc a -> Acc a
  Jmp a -> Nop a

day08 :: IO ()
day08 = do
  input_strs <- lines <$> readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_08.txt"
  case traverse (parse pInstr "input") input_strs of
    Left e -> putStrLn $ errorBundlePretty e
    Right instructions' -> do
      let instructions = V.fromList instructions'
          trackedInstructions = V.zip instructions (V.replicate (V.length instructions) False)
      -- part 1
      putStr "The answer for day eight part one is "
      print =<< part1 =<< V.thaw trackedInstructions
      -- part 2
      putStr "The answer for day eight part two is "
      print =<< part2 trackedInstructions

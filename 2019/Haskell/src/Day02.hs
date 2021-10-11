module Day02 where

import Common
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Intcode

substituteInput :: (a, a) -> [a] -> [a]
substituteInput (a, b) (c : _ : _ : xs) = c : a : b : xs
substituteInput _ _ = undefined

day02 :: IO ()
day02 = do
  input <- readFile "/home/jamie/Git/AdventOfCode/2019/Inputs/day_02.txt"
  let input_instrs = read <$> split ',' input :: [Int]
  -- part 1
  let part1_input = substituteInput (12, 2) input_instrs
  computer <- initIntcode part1_input
  FinishedComputer finalMem _ <- unwrap $ runIntcode computer
  putStr "The solution to day 02 part 01 is "
  print $ finalMem V.! 0

  -- part 2
  let nouns_verbs = do
        a <- [0 .. 99]
        b <- [0 .. 99]
        return (a, b)
      part2_inputs = (`substituteInput` input_instrs) <$> nouns_verbs

  memories <-
    traverse
      (initIntcode >=> (unwrap . runIntcode))
      part2_inputs

  let firsts = V.head . memory' <$> memories
  let x = fromJust $ elemIndex 19690720 firsts
      (noun, verb) = nouns_verbs !! x

  putStr "The solution to day 02 part 02 is "
  print $ 100 * noun + verb

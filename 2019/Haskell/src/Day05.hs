module Day05 where

import Common
import qualified Data.Vector.Unboxed as V
import Intcode

day05 :: IO ()
day05 = do
  input <- readFile "/home/jamie/Git/AdventOfCode/2019/Inputs/day_05.txt"
  let input_instrs = read <$> split ',' input :: [Int]

  pc <- initIntcode input_instrs >>= setInput [1]
  FinishedComputer {..} <- unwrap $ runIntcode pc
  putStr "The solution to day 05 part 01 is "
  print $ V.last output

  pc <- initIntcode input_instrs >>= setInput [5]
  FinishedComputer {..} <- unwrap $ runIntcode pc
  putStr "The solution to day 05 part 02 is "
  print $ V.last output

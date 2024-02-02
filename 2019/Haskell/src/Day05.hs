module Day05 where

import AoC
import Common
import Control.Monad.ST
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as V
import Intcode
import Safe

type Parsed = [Int]

runPart1 :: [Int] -> Int
runPart1 input = runST $ do
  pc <- initIntcode input >>= setInput [1]
  FinishedComputer {..} <- unwrap $ runIntcode pc
  return $ V.last output

runPart2 :: [Int] -> Int
runPart2 input = runST $ do
  pc <- initIntcode input >>= setInput [5]
  FinishedComputer {..} <- unwrap $ runIntcode pc
  return $ V.last output

day05 :: Runner Parsed Int
day05 =
  let year = 2019
      day = 5
      parser :: Text -> Maybe Parsed
      parser input = traverse readMay (split ',' $ Text.unpack input)
      part1 = return . runPart1
      part2 = return . runPart2
   in Runner {..}

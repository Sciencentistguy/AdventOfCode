module Day02 where

import AoC
import Common
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as V
import Intcode
import Safe (readMay)

type Parsed = [Int]

substituteInput :: (a, a) -> [a] -> [a]
substituteInput (a, b) (c : _ : _ : xs) = c : a : b : xs
substituteInput _ _ = undefined

runPart1 :: [Int] -> Int
runPart1 input = runST $ do
  let input' = substituteInput (12, 2) input
  computer <- initIntcode input'
  FinishedComputer finalMem _ <- unwrap $ runIntcode computer
  return $ finalMem V.! 0

runPart2 :: [Int] -> Int
runPart2 input = runST $ do
  let nouns_verbs = do
        a <- [0 .. 99]
        b <- [0 .. 99]
        return (a, b)
      input' = (`substituteInput` input) <$> nouns_verbs
  memories <- traverse (initIntcode >=> (unwrap . runIntcode)) input'
  let firsts = V.head . memory' <$> memories
  let x = fromJust $ elemIndex 19690720 firsts
      (noun, verb) = nouns_verbs !! x
  return $ 100 * noun + verb

day02 :: Runner Parsed Int
day02 =
  let year = 2019
      day = 2
      parser :: Text -> Maybe Parsed
      parser input = traverse readMay (split ',' $ Text.unpack input)
      part1 = return . runPart1
      part2 = return . runPart2
   in Runner {..}

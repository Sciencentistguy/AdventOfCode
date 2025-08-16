{-# LANGUAGE OverloadedStrings #-}

module Day07
  ( day07,
  )
where

import AoC
import AoC.Common (readText)
import Control.Monad (replicateM)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace
import Math.NumberTheory.DirichletCharacters (eval)

data Equation = Equation
  { result :: Int,
    operands :: [Int]
  }

type Parsed = [Equation]

type Solution = Int

parse :: Text -> Maybe Parsed
parse inpt =
  let lines = Text.lines inpt
      f [a, bs] = do
        a <- readText a
        bs <- traverse readText $ Text.splitOn " " bs
        Just $ Equation a bs
      f _ = error "Invalid equation format"
   in traverse f $ Text.splitOn ": " <$> lines

-- arguments: a list of operators (Int -> Int -> Int)
--            a list of operands (Int)
-- evaluates the equation by applying the operators to the operands
--     such that  `evaluate [1, 2, 3] [(+), (*)]` results in `1 + 2 * 3` == `71`
evaluate :: [Int] -> [Int -> Int -> Int] -> Int
evaluate (x : xs) ops = foldl' (\acc (op, y) -> acc `op` y) x (zip ops xs)
evaluate _ _ = error "Not enough operands"

solve :: (Foldable t, Applicative t) => t (Int -> Int -> Int) -> [Equation] -> Int
solve operators =
  sum
    . fmap result
    . filter
      ( \Equation {..} ->
          -- check if any combination of operators produces the correct result.
          -- this should be efficient in cases where multiple operator sequences
          -- produce the right answer
          any
            (\operators -> result == evaluate operands operators)
            (replicateM (length operands - 1) operators) -- generate all combinations of operators
      )

concatInt :: Int -> Int -> Int
concatInt a b =
  let numDigits x
        | x < 10 = 1
        | otherwise = 1 + numDigits (x `div` 10)
      (.^) = (^) :: Int -> Int -> Int -- hack to avoid defaulting to Integer
      multiplier = 10 .^ numDigits b
   in a * multiplier + b

day07 :: Runner Parsed Solution
day07 =
  let year = 2024
      day = 07
      parser :: Text -> Maybe Parsed
      parser = parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . solve [(*), (+)]
      part2 :: Parsed -> Maybe Solution
      part2 = return . solve [(*), (+), concatInt]
   in Runner {..}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03
  ( day03,
  )
where

import AOC
import Common
import Data.Foldable
import Data.Functor
import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [[Bool]]

pBinaryString :: String -> Maybe [Bool]
pBinaryString =
  traverse
    ( \case
        '1' -> return True
        '0' -> return False
        _ -> Nothing
    )

type Number = [Bool]

bStringToInt :: [Bool] -> Int
bStringToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

occurences :: [Number] -> Int -> (Int, Int)
occurences ls i = (length $ filter id ls', length $ filter not ls')
  where
    ls' = (!! i) <$> ls

reduceBitCriteria :: Int -> [Number] -> ([Number], [Number])
reduceBitCriteria len input = go len 0 input input
  where
    go len i o2 co2 | i == len = (o2, co2)
    go len i o2 co2 =
      let (b_o2, a_o2) = occurences o2 i
          (b_co2, a_co2) = occurences co2 i
          o2' = filter (\l -> (l !! i) /= (a_o2 > b_o2)) o2
          co2' = filter (\l -> (l !! i) /= (a_co2 <= b_co2)) co2
       in case (length o2, length co2) of
            (1, 1) -> (o2, co2)
            (x, 1) | x > 1 -> go len (i + 1) o2' co2
            (1, y) | y > 1 -> go len (i + 1) o2 co2'
            (x, y) | x > 1 && y > 1 -> go len (i + 1) o2' co2'
            _ -> undefined

day03 :: Runner Parsed Int
day03 =
  let year = 2021
      day = 3
      parser :: Text -> Maybe Parsed
      parser = traverse pBinaryString . lines . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 input =
        let len = length $ head input
            counts = occurences input <$> [0 .. len -1]
            gamma = uncurry (>) <$> counts
            epsilon = not <$> gamma
         in return $ bStringToInt gamma * bStringToInt epsilon
      part2 input =
        let len = length $ head input
            (o2, co2) = reduceBitCriteria len input
         in return $ bStringToInt (head o2) * bStringToInt (head co2)
   in Runner {..}

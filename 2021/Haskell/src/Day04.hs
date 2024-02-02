{-# LANGUAGE OverloadedStrings #-}

module Day04
  ( day04,
  )
where

import AoC
import Common
import Control.Monad (join)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (find, transpose, (\\))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = ([Int], [Board])

data Board = Board [[Int]] IntSet
  deriving (Show)

pInput :: Parser Parsed
pInput = do
  numbers <- pNumbersLine
  _ <- newline >> newline
  boards <- some do
    b <- pBoard
    _ <- many newline
    return b
  return (numbers, boards)

pNumbersLine :: Parser [Int]
pNumbersLine = sepBy L.decimal (char ',')

pRow :: Parser [Int]
pRow = count 5 do
  _ <- many $ char ' '
  num <- L.decimal
  _ <- many $ char ' '
  return num

pBoard :: Parser Board
pBoard = do
  ints <-
    count 5 do
      x <- pRow
      _ <- newline
      return x
  return $ Board ints mempty

hasWon :: Board -> Bool
hasWon (Board board seen) =
  let rows = board
      cols = transpose board
      r = any (null . (\\ IS.toList seen)) rows
      c = any (null . (\\ IS.toList seen)) cols
   in r || c

applyNumber :: Int -> Board -> Board
applyNumber number (Board board seen) = Board board (IS.insert number seen)

getUnseen :: Board -> [Int]
getUnseen (Board ints seen) = filter (\x -> not $ IS.member x seen) (join ints)

getFirstWinner :: [Int] -> [Board] -> Int
getFirstWinner (number : numbers) boards =
  let boards' = applyNumber number <$> boards
      winner = find hasWon boards'
   in case winner of
        Nothing -> getFirstWinner numbers boards'
        Just winner -> number * sum (getUnseen winner)
getFirstWinner [] _ = unreachable "At least one board should win"

getLastWinner :: [Int] -> [Board] -> Int
getLastWinner (number : numbers) boards =
  let boards' = applyNumber number <$> boards
      yetToWin = filter (not . hasWon) boards'
   in case boards' of
        [winner] | hasWon winner -> number * sum (getUnseen winner)
        _ -> getLastWinner numbers yetToWin
getLastWinner [] _ = unreachable "All boards should win"

day04 :: Runner Parsed Int
day04 =
  let year = 2021
      day = 4
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . parse pInput "input"
      part1 (numbers, boards) = return $ getFirstWinner numbers boards
      part2 (numbers, boards) = return $ getLastWinner numbers boards
   in Runner {..}

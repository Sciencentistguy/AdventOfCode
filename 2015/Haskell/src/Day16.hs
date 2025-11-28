{-# LANGUAGE OverloadedStrings #-}

module Day16
  ( day16,
  )
where

import AoC
import AoC.Common (Parser, unwrapParser)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec (parse, sepBy1)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer qualified as L

data Detectable = Children | Cats | Samoyeds | Pomeranians | Akitas | Vizslas | Goldfish | Trees | Cars | Perfumes deriving (Ord, Eq, Show)

type Reading = Map Detectable Int

data Sue = Sue
  { number :: Int,
    reading :: Reading
  }
  deriving (Show)

targetSue :: Map Detectable Int
targetSue =
  Map.fromList
    [ (Children, 3),
      (Cats, 7),
      (Samoyeds, 2),
      (Pomeranians, 3),
      (Akitas, 0),
      (Vizslas, 0),
      (Goldfish, 5),
      (Trees, 3),
      (Cars, 2),
      (Perfumes, 1)
    ]

pLine :: Parser Sue
pLine = do
  _ <- string "Sue "
  number <- L.decimal
  _ <- string ": "
  reading <- pReading
  return Sue {..}

pReading :: Parser Reading
pReading = Map.fromList <$> sepBy1 pReadingEntry (string ", ")

pReadingEntry :: Parser (Detectable, Int)
pReadingEntry = do
  detectable <- pDetectable
  _ <- string ": "
  number <- L.decimal
  return (detectable, number)

pDetectable :: Parser Detectable
pDetectable =
  string "children" $> Children
    <|> string "cats" $> Cats
    <|> string "samoyeds" $> Samoyeds
    <|> string "pomeranians" $> Pomeranians
    <|> string "akitas" $> Akitas
    <|> string "vizslas" $> Vizslas
    <|> string "goldfish" $> Goldfish
    <|> string "trees" $> Trees
    <|> string "cars" $> Cars
    <|> string "perfumes" $> Perfumes

matchesP1 :: Reading -> Reading -> Bool
matchesP1 target rhs = and $ Map.intersectionWith (==) target rhs

matchesP2 :: Reading -> Reading -> Bool
matchesP2 target rhs =
  let intersecs = Map.toList $ Map.intersectionWith (,) target rhs
   in all (doubleUncurry within) intersecs
  where
    doubleUncurry f (a, (b, c)) = f a b c
    within :: Detectable -> Int -> Int -> Bool
    within detectable = case detectable of
      Cats -> (<)
      Trees -> (<)
      Pomeranians -> (>)
      Goldfish -> (>)
      _ -> (==)

solve :: (Reading -> Reading -> Bool) -> [Sue] -> Int
solve f = number . head . filter (f targetSue . reading)

type Parsed = [Sue]

day16 :: Runner Parsed Int
day16 =
  let year = 2015
      day = 16
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . traverse (parse pLine "(input)") . Text.lines
      part1 :: Parsed -> Maybe Int
      part1 = return . solve matchesP1
      part2 = return . solve matchesP2
   in Runner {..}

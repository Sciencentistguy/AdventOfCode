{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day15
  ( day15,
  )
where

import AoC
import AoC.Common (Parser, unwrapParser)
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (parse, some)
import Text.Megaparsec.Char (alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [Ingredient]

data Ingredient = Ingredient
  { name :: Text,
    capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
  }
  deriving (Show)

pIngredient :: Parser Ingredient
pIngredient = do
  name <- Text.pack <$> some alphaNumChar
  _ <- ": capacity "
  capacity <- L.signed (return ()) L.decimal
  _ <- ", durability "
  durability <- L.signed (return ()) L.decimal
  _ <- ", flavor "
  flavor <- L.signed (return ()) L.decimal
  _ <- ", texture "
  texture <- L.signed (return ()) L.decimal
  _ <- ", calories "
  calories <- L.signed (return ()) L.decimal
  return $ Ingredient {..}

bake :: [Ingredient] -> [Int] -> (Int, Int, Int, Int, Int)
bake ingredients recipe =
  let f property = max 0 $ sum $ zipWith (\i a -> a * property i) ingredients recipe
   in (f capacity, f durability, f flavor, f texture, f calories)

-- a list of all possible combinations of 4 numbers that sum to 100
recipes :: [[Int]]
recipes = [[a, b, c, d] | a <- [0 .. 100], b <- [0 .. 100], c <- [0 .. 100], d <- [0 .. 100], a + b + c + d == 100]

score :: (Num a) => (a, a, a, a, e) -> a
score (a, b, c, d, _) = a * b * c * d

day15 :: Runner Parsed Int
day15 =
  let year = 2015
      day = 15
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . traverse (parse pIngredient "(input)") . Text.lines
      part1 :: Parsed -> Maybe Int
      part1 ings =
        let results = bake ings <$> recipes
         in return $ maximum $ score <$> results
      part2 :: Parsed -> Maybe Int
      part2 ings =
        let results = bake ings <$> recipes
            results' = filter (\x -> x ^. _5 == 500) results
         in return $ maximum $ score <$> results'
   in Runner {..}

{-# LANGUAGE LambdaCase #-}

module Day02 (day02) where

import AoC
import Common (split, trim)
import Data.Bifunctor (Bifunctor (..))
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text as Text

type Parsed = [Bag]

data Bag = Bag {red :: Int, green :: Int, blue :: Int}

power :: Bag -> Int
power Bag{..} = red * green * blue

data Colour = Red | Green | Blue

pColour :: String -> Maybe Colour
pColour = \case
    "red" -> Just Red
    "green" -> Just Green
    "blue" -> Just Blue
    _ -> Nothing

listToPair :: [String] -> Maybe (String, String)
listToPair = \case
    [a, b] -> return (a, b)
    _ -> Nothing

splitToColourPairs :: [Char] -> Maybe [(String, String)]
splitToColourPairs a = traverse listToPair $ fmap (split ' ' . trim) . split ',' =<< split ';' a

pBag :: String -> Maybe Bag
pBag line = do
    idx <- elemIndex ':' line
    pairs <- splitToColourPairs $ drop (idx + 2) line
    parsedPairs <- fmap (first read) <$> traverse (traverse pColour) pairs
    let collectMarbles (num, colour) bag@Bag{..} = case colour of
            Red -> bag{red = max red num}
            Green -> bag{green = max green num}
            Blue -> bag{blue = max blue num}
    return $ foldr collectMarbles (Bag 0 0 0) parsedPairs

day02 :: Runner Parsed Int
day02 =
    let year = 2023
        day = 2
        parser :: Text -> Maybe Parsed
        parser = traverse pBag . lines . Text.unpack
        part1 :: Parsed -> Maybe Int
        part1 = return . sum . fmap fst . filter (\(_, Bag{..}) -> red <= 12 && green <= 13 && blue <= 14) . zip [1 ..]
        part2 :: Parsed -> Maybe Int
        part2 = return . sum . fmap power
     in Runner{..}

{-# LANGUAGE OverloadedStrings #-}

module Day01 (day01) where

import AoC
import Data.Char (digitToInt, isDigit)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.List (foldl')

type Parsed = [Text]

ops :: [(Text, Text)]
ops = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ["o1e", "t2o", "t3ree", "f4ur", "f5ve", "s6x", "s7ven", "e8ght", "n9ne"]

replaceTextualNumbers :: Text -> Text
replaceTextualNumbers text =
    foldl'
        (flip ($))
        text
        (uncurry Text.replace <$> ops)

fixp :: (Eq a) => (a -> a) -> a -> a
fixp f x =
    let x' = f x
     in if x == x'
            then x'
            else f x'

day01 :: Runner Parsed Int
day01 =
    let year = 2023
        day = 1
        parser :: Text -> Maybe Parsed
        parser = return . Text.lines
        part1 :: Parsed -> Maybe Int
        part1 inpt =
            let parsed = fmap digitToInt . filter isDigit . Text.unpack <$> inpt
             in return . sum $
                    parsed <&> do
                        a <- head
                        b <- last
                        return $ a * 10 + b
        part2 :: Parsed -> Maybe Int
        part2 = part1 . fmap (fixp replaceTextualNumbers)
     in Runner{..}

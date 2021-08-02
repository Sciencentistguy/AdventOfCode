module Common where

import Data.List (tails, transpose)
import Data.Maybe
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split onChar toSplit = before : split onChar (drop 1 after)
  where
    (before, after) = span (/= onChar) toSplit

countCharString :: String -> Char -> Int
countCharString str c = countTrue (== c) str

countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p

groupEntries :: [String] -> [String]
groupEntries [] = []
groupEntries strings = unwords group : groupEntries (drop (length group + 1) strings)
  where
    group = takeWhile (not . null) strings

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = mapMaybe

unreachable :: String -> a
unreachable msg =
  error $
    "ERROR: Entered unreachable code."
      ++ if not $ null msg
        then ' ' : msg
        else ""

type Parser = Parsec Void String

spaces :: Parser ()
spaces =
  L.space
    space1
    (return ())
    (return ())

symbol :: String -> Parser String
symbol = L.symbol spaces

windows :: Int -> [a] -> [[a]]
windows n xs = transpose $ take n $ tails xs

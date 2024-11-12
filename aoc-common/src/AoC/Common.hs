module AoC.Common where

import Data.Foldable (Foldable (toList))
import Data.List (tails, transpose)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split onChar toSplit = before : split onChar (drop 1 after)
  where
    (before, after) = span (/= onChar) toSplit

countOccurences :: (Foldable t, Eq a) => t a -> a -> Int
countOccurences str c = countTrue (== c) str

countTrue :: (Foldable t) => (a -> Bool) -> t a -> Int
countTrue p = length . filter p . toList

groupEntries :: [String] -> [String]
groupEntries [] = []
groupEntries strings = unwords group : groupEntries (drop (length group + 1) strings)
  where
    group = takeWhile (not . null) strings

unreachable :: String -> a
unreachable msg =
  error $
    "ERROR: Entered unreachable code."
      ++ if not $ null msg
        then ' ' : msg
        else ""

type Parser = Parsec Void Text

spaces :: Parser ()
spaces =
  L.space
    space1
    (return ())
    (return ())

symbol :: Text -> Parser Text
symbol = L.symbol spaces

windows :: Int -> [a] -> [[a]]
windows n xs = transpose $ take n $ tails xs

unzipWith :: (Functor f) => (a -> b -> c) -> f (a, b) -> f c
unzipWith f = fmap (uncurry f)

unwrapParser ::
  (Monad m, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Either (ParseErrorBundle s e) a ->
  m a
unwrapParser p = case p of
  Right x -> return x
  Left e -> error $ errorBundlePretty e

envelop :: (b -> [a] -> (b, [a])) -> b -> [a] -> b
envelop _ a [] = a
envelop f a xs = uncurry (envelop f) (f a xs)

tiles :: Int -> Int -> [a] -> [[a]]
tiles size skip = envelop tile []
  where
    tile partial rest
      | length rest < size = (partial, [])
      | otherwise = (partial ++ [take size rest], drop skip rest)

modifyAtN :: (a -> a) -> Int -> [a] -> [a]
modifyAtN f idx ls
  | idx < 0 || idx >= length ls = ls
  | otherwise = take idx ls ++ [f (ls !! idx)] ++ drop (idx + 1) ls

runNTimes :: Int -> (a -> a) -> a -> a
runNTimes n f x
  | n <= 0 = x
  | otherwise = runNTimes (n - 1) f (f x)

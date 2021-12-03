module Common where

import Control.Arrow
import Data.Foldable
import Data.Foldable (Foldable (toList))
import Data.Function
import Data.List
import Data.List (tails, transpose)
import Data.Maybe
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split onChar toSplit = before : split onChar (drop 1 after)
  where
    (before, after) = span (/= onChar) toSplit

countOccurences :: (Foldable t, Eq a) => t a -> a -> Int
countOccurences str c = countTrue (== c) str

countTrue :: Foldable t => (a -> Bool) -> t a -> Int
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

unzipWith :: Functor f => (a -> b -> c) -> f (a, b) -> f c
unzipWith f = fmap (uncurry f)

unwrapParser ::
  (Monad m, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Either (ParseErrorBundle s e) a ->
  m a
unwrapParser p = case p of
  Right x -> return x
  Left e -> error $ errorBundlePretty e

mostCommon :: Ord a => [a] -> a
mostCommon list = fst . maximumBy (compare `on` snd) $ elemCount
  where
    elemCount = map (head &&& length) . group . sort $ list

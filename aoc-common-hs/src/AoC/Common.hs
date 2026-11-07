module AoC.Common where

import Control.Applicative (ZipList (..))
import Data.Foldable (Foldable (toList))
import Data.List (tails)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
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

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

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

-- | Return all combinations of a sequence of values.
combinations :: [a] -> [[a]]
combinations coins = concat [combs i coins | i <- [1 .. length coins]]
  where
    combs 0 _ = [[]]
    combs _ [] = []
    combs k (x : xs) = map (x :) (combs (k - 1) xs) ++ combs k xs

parseLines :: Parsec e Text b -> Text -> Either (ParseErrorBundle Text e) [b]
parseLines p = traverse (parse p "(input)") . Text.lines

absdiff :: Int -> Int -> Int
absdiff a b = abs (a - b)

ok :: Either a1 a2 -> Maybe a2
ok (Right x) = Just x
ok _ = Nothing

readText :: Text -> Maybe Int
readText = fmap fst . ok . decimal

initN :: Int -> [b] -> [b]
initN n ls = fst <$> zip ls (drop n ls)

-- take a function, and a value, and returns the value of the fixed point
fixp :: (Eq a) => (a -> a) -> a -> a
fixp f x = let x' = f x in if x' == x then x else fixp f x'

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 :: Int ..]

-- unwrapDecimal :: Either a1 (a2, b) -> a2
unwrapDecimal :: (Show a1, Show a2, Show b) => Either a1 (a2, b) -> a2
unwrapDecimal x = case x of
  Right (a, _) -> a
  e -> error $ "unwrap: " ++ show e

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose k (x : xs) = map (x :) (choose (k - 1) xs) ++ choose k xs

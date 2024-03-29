module Common where

import Control.Arrow (Arrow ((&&&)))
import Data.Foldable (Foldable (toList), maximumBy)
import Data.Function (on)
import Data.List (group, sort, tails, transpose, unfoldr)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
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

pointsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsBetween pa@(xa, ya) pb@(xb, yb) = map maySwitch . unfoldr go $ (x1, y1, 0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x, y) -> (y, x)) else id
    [(x1, y1), (x2, y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
      | xTemp > x2 = Nothing
      | otherwise = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
      where
        tempError = error + deltay
        (newY, newError) =
          if (2 * tempError) >= deltax
            then (yTemp + ystep, tempError - deltax)
            else (yTemp, tempError)

module Day05
  ( day05,
  )
where

import AOC
import Common
import Control.Applicative (liftA2, liftA3)
import Data.Functor (($>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text

type Parsed = [String]

(&&&) :: Bool -> Bool -> Bool -> Bool
(&&&) a b c = a && b && c

isNice :: String -> Bool
isNice = liftA3 (&&&) hasThreeVowels hasLetterPair hasNoBannedPairs

hasLetterPair :: String -> Bool
hasLetterPair s = any (`isInfixOf` s) letterPairs
  where
    letterPairs = [a : [a] | a <- ['a' .. 'z']]

hasThreeVowels :: String -> Bool
hasThreeVowels s = length (filter (`elem` "aeiou") s) >= 3

hasNoBannedPairs :: String -> Bool
hasNoBannedPairs s = not $ any (`isInfixOf` s) bannedPairs
  where
    bannedPairs = ["ab", "cd", "pq", "xy"]

isNicer :: String -> Bool
isNicer = liftA2 (&&) hasMatchingPair hasXyxs

hasMatchingPair :: String -> Bool
hasMatchingPair s = any ((>= 2) . length) $ group $ sort $ tiles 2 1 $ concatMap shortenThrees $ group s
  where
    shortenThrees [x, _, _] = [x, x]
    shortenThrees xs = xs

hasXyxs :: String -> Bool
hasXyxs s =
  any xyx $ tiles 3 1 s
  where
    xyx [c1, _, c3] = c1 == c3
    xyx _ = error "bad tile length"

day05 :: Runner Parsed Int
day05 =
  let year = 2015
      day = 5
      parser :: Text -> Maybe Parsed
      parser = return . fmap Text.unpack . Text.lines
      part1 = return . length . filter isNice
      part2 = return . length . filter isNicer
   in Runner {..}

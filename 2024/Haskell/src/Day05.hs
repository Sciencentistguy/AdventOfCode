{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day05
  ( day05,
  )
where

import AoC
import AoC.Common
import Control.Lens ((&))
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortBy)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)

type Page = Int

type Rules = HashMap Page (HashSet Page)

type Update = [Page]

type Parsed = (Rules, [Update])

type Solution = Int

testInpt =
  Text.unlines
    [ "47|53",
      "97|13",
      "97|61",
      "97|47",
      "75|29",
      "61|13",
      "75|53",
      "29|13",
      "97|29",
      "53|29",
      "61|53",
      "97|53",
      "61|29",
      "47|13",
      "75|47",
      "97|75",
      "47|61",
      "75|61",
      "47|29",
      "75|13",
      "53|13",
      "",
      "75,47,61,53,29",
      "97,61,53,29,13",
      "75,29,13",
      "75,97,47,61,53",
      "61,13,29",
      "97,13,75,29,47"
    ]

parse :: Text -> Maybe Parsed
parse inpt = do
  let [rules, updates] = Text.splitOn "\n\n" inpt
      -- Updates are trivial
      updates' = fmap (read . Text.unpack) . Text.splitOn "," <$> Text.lines updates

  -- Parse each rule line into a pair of Ints
  rulePairs <- traverse parseRule $ Text.lines rules
  -- Create a map from each page to the set of pages after it
  let rulesMap =
        foldl
          ( \acc (a, b) ->
              HM.alter
                ( \case
                    Just s -> Just $ HS.insert b s
                    Nothing -> Just $ HS.singleton b
                )
                a
                acc
          )
          HM.empty
          rulePairs
  return (rulesMap, updates')
  where
    parseRule :: Text -> Maybe (Page, Page)
    parseRule line = case Text.splitOn "|" line of
      [a, b] -> ok do
        (a', _) <- decimal a
        (b', _) <- decimal b
        return (a', b')
      _ -> Nothing

middle :: [Page] -> Page
middle xs = xs !! (length xs `div` 2)

pairWindows :: [a] -> [(a, a)]
pairWindows xs = zip xs $ drop 1 $ xs

-- ensure that for each pair (a, b) in windows 2 update, rules[a] contains b
ordered :: Rules -> Update -> Bool
ordered rules update =
  pairWindows update
    & all
      ( \(a, b) -> case HM.lookup a rules of
          Just rule -> HS.member b rule
          Nothing -> False
      )

orderingCmp :: Rules -> Page -> Page -> Ordering
orderingCmp rules a b = case HM.lookup a rules of
  (Just rule) -> if HS.member b rule then LT else GT
  _ -> EQ

reorder :: Rules -> Update -> Update
reorder rules update = sortBy (orderingCmp rules) update

day05 :: Runner Parsed Solution
day05 =
  let year = 2024
      day = 05
      parser :: Text -> Maybe Parsed
      parser = parse
      part1 :: Parsed -> Maybe Solution
      part1 (rules, updates) = return $ sum $ middle <$> filter (ordered rules) updates
      part2 :: Parsed -> Maybe Solution
      part2 (rules, updates) =
        return $
          sum $
            middle . reorder rules <$> filter (not . ordered rules) updates
   in Runner {..}

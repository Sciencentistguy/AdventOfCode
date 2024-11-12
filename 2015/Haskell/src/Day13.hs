{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Day13
  ( day13,
  )
where

import AoC
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Safe

type Parsed = [Person]

data Person = Person
  { name :: Text,
    relationships :: Map Text Int
  }
  deriving (Show, Eq)

pLines :: Text -> Maybe [Person]
pLines = traverse pLine . Text.lines

pLine :: Text -> Maybe Person
pLine line = do
  [name, _, sign', num, _, _, _, _, _, _, other'] <- return $ Text.split (== ' ') line
  val <- readMay (Text.unpack num) :: Maybe Int
  sign <- case sign' of
    "gain" -> Just 1 :: Maybe Int
    "lose" -> Just $ -1
    _ -> Nothing
  let relationship = val * sign
      other = Text.init other'
      relationships = Map.insert other relationship mempty
  return $ Person {..}

(+?) :: Person -> Person -> Maybe Person
p1 +? p2 =
  if name p1 == name p2
    then return $ Person (name p1) $ relationships p1 <> relationships p2
    else Nothing

mergePeople :: [Person] -> Maybe Person
mergePeople (x : xs) = foldM (+?) x xs
mergePeople [] = Nothing

splitPeoples :: [Person] -> Map Text [Person]
splitPeoples peoples = go peoples mempty
  where
    go (person@(Person name _) : xs) acc =
      let acc' = Map.alter f name acc
          f existing = return case existing of
            Nothing -> [person]
            Just ls -> person : ls
       in go xs acc'
    go [] acc = acc

getRelationshipTo :: Person -> Person -> Maybe Int
getRelationshipTo (Person _ relationships) (Person otherName _) = Map.lookup otherName relationships

getHappinessFromOrder :: [Person] -> Maybe Int
getHappinessFromOrder people = do
  let f me (l, r) = do
        relToLeft <- getRelationshipTo me l
        relToRight <- getRelationshipTo me r
        return $ relToLeft + relToRight
      ls !@ idx = ls !! (idx `mod` length people)
      getLR i = (people !@ (i + 1), people !@ (i - 1))
      g i me = f me (getLR i)
  sum <$> traverse (uncurry g) (zip [0 ..] people)

addMe :: Parsed -> Parsed
addMe people =
  let me = Person "Me" $ Map.fromList [(name p, 0) | p <- people]
      rest = people <&> \p -> p {relationships = Map.insert "Me" 0 (relationships p)}
   in me : rest

day13 :: Runner Parsed Int
day13 =
  let year = 2015
      day = 13
      parser :: Text -> Maybe Parsed
      parser text = toList <$> (pLines text >>= traverse mergePeople . splitPeoples)
      part1 :: Parsed -> Maybe Int
      part1 parsed = maximum <$> traverse getHappinessFromOrder (permutations parsed)
      part2 :: Parsed -> Maybe Int
      part2 = part1 . addMe
   in Runner {..}

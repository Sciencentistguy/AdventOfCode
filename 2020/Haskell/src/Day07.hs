{-# LANGUAGE OverloadedStrings #-}

module Day07 (day07) where

import AOC
import Algebra.Graph.HigherKinded.Class (Graph)
import qualified Algebra.Graph.Labelled as GL
import qualified Algebra.Graph.ToGraph as G
import Common
import Data.Char (digitToInt, isSpace)
import Data.Function ((&))
import Data.List (dropWhileEnd)
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = GL.Graph (Sum Int) Bag

type Bag = String

data Rule a = Rule
  { outerRule :: a,
    innerRules :: [(a, Int)]
  }
  deriving (Show)

pRules :: Parser [Rule Bag]
pRules = sepEndBy pRule newline

pRule :: Parser (Rule Bag)
pRule = do
  outer <- space >> manyTill anySingle (string "bags")
  _ <- space1 >> string "contain"
  inner <- pInnerBags
  _ <- space >> char '.'
  return $ Rule (dropWhileEnd isSpace outer) inner

pInnerBags :: Parser [(Bag, Int)]
pInnerBags = space >> (noBags <|> innerBags)
  where
    noBags = string "no other bags" >> return []
    innerBags = pInnerBag `sepBy1` (space >> char ',')

pInnerBag :: Parser (Bag, Int)
pInnerBag = do
  n <- space >> L.decimal
  col <- space1 >> manyTill anySingle bags
  return (dropWhileEnd isSpace col, n)
  where
    bags = string "bag" >> optional (char 's')

buildGraph :: [Rule Bag] -> GL.Graph (Sum Int) Bag
buildGraph rules = GL.edges $ do
  Rule outer inners <- rules
  (inner, n) <- inners
  return (Sum n, outer, inner)

canContain :: Bag -> GL.Graph (Sum Int) Bag -> S.Set Bag
canContain bag graph =
  reachable & if nonTrivLoopExists then id else S.delete bag
  where
    reachable = S.fromList $ G.reachable bag (GL.transpose graph)
    nonTrivLoopExists = any nonTrivLoop (G.postSet bag graph)
    nonTrivLoop v = bag `elem` G.reachable v graph

mustContain :: Bag -> GL.Graph (Sum Int) Bag -> Int
mustContain bag graph = go bag - 1
  where
    go u =
      let next = G.postSet u graph
          contrib v =
            let Sum label = GL.edgeLabel u v graph
             in label * go v
       in 1 + foldr (\v acc -> acc + contrib v) 0 next

day07 :: Runner Parsed Int
day07 =
  let year = 2020
      day = 7
      parser input = case parse pRules "input" input of
        Left e -> error $ errorBundlePretty e
        Right x -> return $ buildGraph x
      part1 = return . S.size . canContain "shiny gold"
      part2 = return . mustContain "shiny gold"
   in Runner {..}

module Day04
  ( day04,
  )
where

import AoC
import Control.Monad (guard)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector.Internal.Check (check)

type Parsed = HM.HashMap Vec2D Char

type Solution = Int

data Vec2D = Vec2D
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

instance Hashable Vec2D where
  hashWithSalt salt (Vec2D x y) = salt `hashWithSalt` (476743 * hash x + 152857 * hash y)

instance Num Vec2D where
  (Vec2D x1 y1) + (Vec2D x2 y2) = Vec2D (x1 + x2) (y1 + y2)
  (Vec2D x1 y1) - (Vec2D x2 y2) = Vec2D (x1 - x2) (y1 - y2)
  (Vec2D x1 y1) * (Vec2D x2 y2) = Vec2D (x1 * x2) (y1 * y2)
  abs (Vec2D x y) = Vec2D (abs x) (abs y)
  signum (Vec2D x y) = Vec2D (signum x) (signum y)
  fromInteger n = Vec2D (fromInteger n) (fromInteger n)

-- Constants

word :: String
word = "XMAS"

directions :: [Vec2D]
directions =
  [ Vec2D 0 1, -- up
    Vec2D 0 (-1), -- down
    Vec2D 1 0, -- right
    Vec2D (-1) 0, -- left
    Vec2D 1 1, -- up-right
    Vec2D (-1) 1, -- up-left
    Vec2D 1 (-1), -- down-right
    Vec2D (-1) (-1) -- down-left
  ]

corners :: [Vec2D]
corners =
  [ Vec2D (1) (-1), -- top right
    Vec2D (1) (1), -- top left
    Vec2D (-1) (-1), -- bottom right
    Vec2D (-1) (1) -- bottom left
  ]

checkWord :: Parsed -> Vec2D -> Vec2D -> Bool
checkWord map current' direction = go word current'
  where
    go (ch : xs) current = case map HM.!? current of
      Just mapCh | mapCh == ch -> go xs (current + direction)
      _ -> False
    go [] _ = True

p1 :: Parsed -> Int
p1 map = foldl' f 0 (HM.toList map)
  where
  first = head word
  f :: Int -> (Vec2D, Char) -> Int
  f acc (coord, letter) =
    if letter == first
      then acc + (length $ filter (checkWord map coord) directions)
      else acc

p2 map = length $ filter f $ HM.toList map
  where
    m_and_s 'M' 'S' = True
    m_and_s 'S' 'M' = True
    m_and_s _ _ = False

    g coord letter = do
      guard $ letter == 'A'
      let directions = (coord +) <$> corners
      [tl, tr, bl, br] <- (map HM.!?) `traverse` directions
      return $ m_and_s tl br && m_and_s tr bl

    f (coord, letter) = fromMaybe False (g coord letter)

parse :: Text -> HM.HashMap Vec2D Char
parse input =
  let lines = Text.unpack <$> Text.lines input
   in HM.fromList do
      (y, line) <- zip [0 ..] lines
      (x, ch) <- zip [0 ..] line
      return (Vec2D x y, ch)

day04 :: Runner Parsed Solution
day04 =
  let year = 2024
      day = 04
      parser :: Text -> Maybe Parsed
      parser = return . parse
      part1 :: Parsed -> Maybe Solution
      part1 = return . p1
      part2 :: Parsed -> Maybe Solution
      part2 = return . p2
   in Runner {..}

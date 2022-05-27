module Day10
  ( day10,
  )
where

import AOC
import Common
import Control.Applicative
import Control.Monad ((<=<))
import Data.List (group)
import Data.Text (Text)
import qualified Data.Text as Text
import Safe

type Parsed = String

lookAndSay :: String -> String
lookAndSay = liftA2 (++) (show . length) (take 1) <=< group

day10 :: Runner Parsed Int
day10 =
  let year = 2015
      day = 10
      parser :: Text -> Maybe Parsed
      parser = return . init . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 inpt = return $ length $ iterate lookAndSay inpt !! 40
      part2 inpt = return $ length $ iterate lookAndSay inpt !! 50
   in Runner {..}

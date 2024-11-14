{-# LANGUAGE OverloadedStrings #-}

module Day17
  ( day17,
  )
where

import AoC
import AoC.Common (Parser, combinations, unwrapParser)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (parse)
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [Int]

solutions :: (Eq a, Num a) => [a] -> [[a]]
solutions conts = filter (\x -> sum x == 150) $ combinations conts

day17 :: Runner Parsed Int
day17 =
  let year = 2015
      day = 17
      parser :: Text -> Maybe Parsed
      parser = unwrapParser . traverse (parse (L.decimal :: Parser Int) "(input)") . Text.lines
      part1 :: Parsed -> Maybe Int
      part1 = return . length . solutions
      part2 :: Parsed -> Maybe Int
      part2 containers =
        let solutions' = solutions containers
            shortest = minimum $ length <$> solutions'
            shortests = filter (\ls -> length ls == shortest) solutions'
         in return $ length shortests
   in Runner {..}

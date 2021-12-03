module Day02
  ( day02,
  )
where

import AOC
import Common
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parsed = [PasswordSpec]

data PasswordSpec = PasswordSpec
  { psFirstNum :: Int,
    psSecondNum :: Int,
    psChar :: Char,
    psString :: String
  }

-- TODO: common
pPassword :: Parsec Void Text PasswordSpec
pPassword = do
  psFirstNum <- L.decimal
  _ <- char '-'
  psSecondNum <- L.decimal
  _ <- char ' '
  psChar <- letterChar
  _ <- char ':'
  _ <- char ' '
  psString <- some letterChar
  return PasswordSpec {..}

isValidPartOne :: PasswordSpec -> Bool
isValidPartOne PasswordSpec {..} =
  let count = countOccurences psString psChar
   in count >= psFirstNum && count <= psSecondNum

isValidPartTwo :: PasswordSpec -> Bool
isValidPartTwo PasswordSpec {..} =
  let pos1 = psString !! (psFirstNum - 1)
      pos2 = psString !! (psSecondNum - 1)
   in (pos1 == psChar) /= (pos2 == psChar)

day02 :: Runner Parsed Int
day02 =
  let day = 02
      year = 2020
      parser input = unwrapParser $ traverse (parse pPassword "input") (Text.lines input)
      part1 = return . length . filter isValidPartOne
      part2 = return . length . filter isValidPartTwo
   in Runner {..}

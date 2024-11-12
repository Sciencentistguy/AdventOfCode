module Day11
  ( day11,
  )
where

import AoC
import AoC.Common
import Control.Applicative
import Control.Monad ((<=<))
import Data.Char (chr, ord)
import Data.Functor ((<&>))
import Data.List (find, group)
import Data.Text (Text)
import qualified Data.Text as Text
import Safe

type Parsed = String

firstReq :: String -> Bool
firstReq pw = any ascending $ tiles 3 1 pw
  where
    ascending tile =
      let [c1, c2, c3] = ord <$> tile
       in c3 == c2 + 1
            && c2 == c1 + 1

secondReq :: String -> Bool
secondReq pw = and $ notElem <$> ['i', 'j', 'k'] <*> return pw

thirdReq :: String -> Bool
thirdReq pw = (>= 2) $ sum $ (`div` 2) . length <$> group pw

nextPassword :: String -> String
nextPassword pw =
  fst $ foldr incrChar ("", 1) pw
  where
    incrChar c (cs, carry) =
      let c' = ord c + carry - ord 'a'
       in ( chr ((c' `mod` 26) + ord 'a') : cs,
            0 `max` (c' - 25)
          )

isValid :: String -> Bool
isValid =
  liftA3
    (\a b c -> a && b && c)
    firstReq
    secondReq
    thirdReq

validPasswords :: String -> [String]
validPasswords start = filter isValid (passwords start)
  where
    passwords :: String -> [String]
    passwords = iterate nextPassword

day11 :: Runner Parsed String
day11 =
  let year = 2015
      day = 11
      parser :: Text -> Maybe Parsed
      parser = return . init . Text.unpack
      part1 :: Parsed -> Maybe String
      part1 inpt = validPasswords inpt `atMay` 0
      part2 inpt = validPasswords inpt `atMay` 1
   in Runner {..}

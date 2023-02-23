module Day03 (day03) where

import AoC
import Control.Applicative
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

type Parsed = [(String, String)]

priority :: Char -> Maybe Int
priority c
  | c `elem` ['a' .. 'z'] = Just $ fromEnum c - 96
  | c `elem` ['A' .. 'Z'] = Just $ fromEnum c - 38
  | otherwise = Nothing

part1' :: (String, String) -> Int
part1' (first, second) =
  let f c =
        if c `elem` second
          then priority c
          else Nothing
   in sum $ nub $ mapMaybe f first

part2' :: [(String, String)] -> Int
part2' [first, second, third] =
  let f pred =
        listToMaybe $
          intersect
            (filter pred $ uncurry (++) second)
            (filter pred $ uncurry (++) third)
   in case f (`elem` fst first) <|> f (`elem` snd first) of
        Nothing -> 0
        Just x -> fromJust $ priority x
part2' _ = error "bad input"

day03 :: Runner Parsed Int
day03 =
  let year = 2022
      day = 3
      parser :: Text -> Maybe Parsed
      parser =
        let half :: [x] -> ([x], [x])
            half x = splitAt (length x `div` 2) x
         in return . fmap half . lines . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 = return . sum . fmap part1'
      part2 = return . sum . fmap part2' . chunksOf 3
   in Runner {..}
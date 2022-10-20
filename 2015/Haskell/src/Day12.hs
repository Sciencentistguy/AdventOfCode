{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day12
  ( day12,
  )
where

import AOC
import Common
import Control.Applicative
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isDigit)
import Data.Function
import Data.List (groupBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Scientific (Scientific)
import qualified Data.Scientific as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Safe

type Parsed = Value

collectNums :: (Object -> Bool) -> Value -> Scientific
collectNums shouldCollect = fix \go -> \case
  Object o ->
    if shouldCollect o
      then sum $ go <$> o
      else 0
  Array a -> sum $ go <$> a
  Number n -> n
  _ -> 0

day12 :: Runner Parsed Integer
day12 =
  let year = 2015
      day = 12
      parser :: Text -> Maybe Parsed
      parser = decode' . BSL.fromStrict . Text.encodeUtf8
      part1 :: Parsed -> Maybe Integer
      part1 inpt = case S.floatingOrInteger $ collectNums (const True) inpt of
        Left _ -> Nothing
        Right i -> Just i
      part2 inpt =
        let shouldCollect v = not (KM.member "red" v) && ("red" `notElem` KM.elems v)
         in case S.floatingOrInteger $ collectNums shouldCollect inpt of
              Left _ -> Nothing
              Right i -> Just i
   in Runner {..}

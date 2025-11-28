{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day12
  ( day12,
  )
where

import AoC
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.Scientific (Scientific)
import Data.Scientific qualified as S
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

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

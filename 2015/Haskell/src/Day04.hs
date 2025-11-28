module Day04
  ( day04,
  )
where

import AoC
import Crypto.Hash qualified as Hash
import Data.Bits (shiftR, (.&.))
import Data.ByteArray qualified as BA
import Data.ByteString.Char8 qualified as BC
import Data.Char (intToDigit)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Word (Word8)

type Parsed = String

byteHex :: Word8 -> String
byteHex b = intToDigit <$> [fromIntegral b `shiftR` 4, fromIntegral b .&. 0xf]

showHex :: [Word8] -> String
showHex = (=<<) byteHex

hashString :: String -> String
hashString s = showHex $ BA.unpack $ Hash.hashWith Hash.MD5 $ BC.pack s

solve :: Int -> String -> Maybe Int
solve numDigits input = find valid [1 ..]
  where
    valid n = replicate numDigits '0' == take numDigits (hashString $ input ++ show n)

day04 :: Runner Parsed Int
day04 =
  let year = 2015
      day = 4
      parser :: Text -> Maybe Parsed
      parser = return . init . Text.unpack
      part1 = solve 5
      part2 = solve 6
   in Runner {..}

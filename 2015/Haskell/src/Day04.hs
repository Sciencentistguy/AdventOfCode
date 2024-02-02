module Day04
  ( day04,
  )
where

import AoC
import qualified Crypto.Hash as Hash
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString.Char8 as BC
import Data.Char (intToDigit)
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Word (Word8)
import Crypto.Hash (Digest)
import qualified Data.ByteArray as BA

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

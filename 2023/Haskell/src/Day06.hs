module Day06 (day06) where

import AoC
import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

type Parsed = ([Race], Race)

data Race = Race
    { time :: Int
    , distance :: Int
    }
    deriving (Show)

readText :: Text -> Maybe Int
readText = readMaybe . Text.unpack

p1Parser :: Text -> Maybe [Race]
p1Parser inpt = do
    [time, distance] <- return $ Text.lines inpt
    let
        times = mapMaybe readText $ Text.split (== ' ') $ Text.drop 5 time
        distances = mapMaybe readText $ Text.split (== ' ') $ Text.drop 9 distance
        ret = zipWith Race times distances
    return ret

p2Parser :: Text -> Maybe Race
p2Parser inpt = do
    let concatDigits :: Text -> Maybe Int
        concatDigits = readText . Text.filter isDigit
    [time, distance] <- traverse concatDigits $ Text.lines inpt
    return Race{..}

totalWins :: Race -> Int
totalWins Race{..} =
    let b = fromIntegral time :: Double
        c = fromIntegral distance :: Double
        disc = sqrt $ (b * b) - (4 * c)
        epsilon = 0.0001
        upper = floor $ (b + disc) / 2 - epsilon
        lower = ceiling $ (b - disc) / 2 + epsilon
     in upper - lower + 1

day06 :: Runner Parsed Int
day06 =
    let year = 2023
        day = 6
        parser :: Text -> Maybe Parsed
        parser = uncurry (liftA2 (,)) <$> (p1Parser &&& p2Parser)
        part1 :: Parsed -> Maybe Int
        part1 = return . product . fmap totalWins . fst
        part2 :: Parsed -> Maybe Int
        part2 = return . totalWins . snd
     in Runner{..}

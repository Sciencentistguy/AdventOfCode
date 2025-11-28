{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Day05 (day05) where

import AoC
import Common (ok)
import Common.Range (Range (..))
import Common.Range qualified as Range
import Control.Monad (MonadPlus, guard)
import Control.Monad.ST
import Data.Foldable (foldlM, for_)
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.Split
import Data.Maybe (mapMaybe, maybeToList)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import Data.Vector qualified as V

type Parsed = Almanac

data Almanac = Almanac
    { seeds :: [Int]
    , mappings :: [[Mapping]]
    }
    deriving (Show)

data Mapping = Mapping
    { range :: Range
    , offset :: Int
    }
    deriving (Show)

data MappedRange = MappedRange
    { before :: Maybe Range
    , overlap :: Maybe Range
    , after :: Maybe Range
    }
    deriving (Show)

translate :: [Mapping] -> Int -> Int
translate mappings x = case find ((`Range.contains` x) . range) mappings of
    Just Mapping{..} -> x + offset
    Nothing -> x

mapRange :: Mapping -> Range -> MappedRange
mapRange (Mapping (Range selfStart selfEnd) selfOffset) Range{..} =
    let optRange start end = do
            let x = Range.Range{..}
            guard $ not $ Range.null x
            return x
        before = optRange start (min selfStart end)
        after = optRange (max selfEnd start) end
        overlap =
            optRange (max start selfStart) (min end selfEnd) <&> \Range{..} ->
                Range (start + selfOffset) (end + selfOffset)
     in MappedRange{..}

translateRange :: (Foldable t) => Range -> t Mapping -> [Range]
translateRange inputRange mappings = runST do
    pOverlaps <- newSTRef $ V.fromList []
    pLeftovers <- newSTRef $ V.fromList [inputRange]
    pLeftovers_queue <- newSTRef $ V.fromList []
    mappings `for_` \mapping -> do
        leftovers <- readSTRef pLeftovers
        leftovers `for_` \inputRange -> do
            let MappedRange{..} = mapRange mapping inputRange
            modifySTRef' pOverlaps (++? overlap)
            modifySTRef' pLeftovers_queue (++? before)
            modifySTRef' pLeftovers_queue (++? after)
        swapSTRef pLeftovers pLeftovers_queue
        writeSTRef pLeftovers_queue $ V.fromList []
    overlaps <- readSTRef pOverlaps
    leftovers <- readSTRef pLeftovers
    return $ V.toList $ overlaps V.++ leftovers

swapSTRef :: STRef s a -> STRef s a -> ST s ()
swapSTRef pA pB = do
    aVal <- readSTRef pA
    bVal <- readSTRef pB
    writeSTRef pA bVal
    writeSTRef pB aVal

(++?) :: V.Vector a -> Maybe a -> V.Vector a
ls ++? x = ls V.++ V.fromList (maybeToList x)

part2' :: Almanac -> Int
part2' Almanac{..} =
    let seedRanges =
            flip mapMaybe (chunksOf 2 seeds) \x -> do
                [a, b] <- return x
                return $ Range a (a + b)
     in minimum $
            Range.start <$> do
                seedRange <- seedRanges
                foldlM translateRange seedRange mappings

pAlmanac :: (MonadPlus m, MonadFail m) => Text -> m Almanac
pAlmanac inpt = do
    let sections = Text.splitOn "\n\n" inpt
        mappings_ = tail sections
    seeds <- ok $ traverse (fmap fst . Text.decimal) $ Text.split (== ' ') $ Text.drop 7 $ head sections
    let x = tail . Text.lines <$> mappings_
    mappings <- traverse (traverse pMapping) x
    return Almanac{..}

pMapping :: (MonadPlus m, MonadFail m) => Text -> m Mapping
pMapping line = do
    [dstStart, srcStart, length] <- ok $ traverse (fmap fst . Text.decimal) $ Text.split (== ' ') line
    let offset = dstStart - srcStart
    let end = srcStart + length
    let start = srcStart
    let range = Range.Range{..}
    return Mapping{..}

day05 :: Runner Parsed Int
day05 =
    let year = 2023
        day = 5
        parser :: Text -> Maybe Parsed
        parser = pAlmanac
        part1 :: Parsed -> Maybe Int
        part1 Almanac{..} =
            let f seed = foldl' (flip translate) seed mappings
             in return $ minimum $ f <$> seeds
        part2 :: Parsed -> Maybe Int
        part2 = return . part2'
     in Runner{..}

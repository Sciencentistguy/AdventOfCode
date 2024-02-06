module Common.Range where

data Range = Range {start :: Int, end :: Int}

inclusive :: Int -> Int -> Range
inclusive a b = Range a (b + 1)

contains :: Range -> Int -> Bool
contains Range{..} x = x >= start && x < end

null :: Range -> Bool
null Range{..} = start >= end

instance Show Range where
    show Range{..} = "Range {" ++ show start ++ ".." ++ show end ++ "}"

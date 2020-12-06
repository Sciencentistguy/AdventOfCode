module DaySix
    ( daySix
    )
where

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.List
import           Common

daySix :: IO ()
daySix = do
    input_Text <- Text.lines <$> Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_six.txt"
    let input_strs = map Text.unpack input_Text
    let grouped    = groupEntries input_strs
    -- part 1
    let filtered   = map (filter (/= ' ')) grouped
    let anyYes     = map nub filtered
    let counts     = map length anyYes
    putStr "The answer for day six part one is "
    print $ sum counts
    -- part 2
    putStr "The answer for day six part two is "
    let splitted = map (split ' ') grouped
    let allYes   = map (foldl intersect ['a' .. 'z']) splitted
    let counts   = map length allYes
    print $ sum counts

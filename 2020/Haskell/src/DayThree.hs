module DayThree
    ( dayThree
    )
where

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

type Map = [String]

isTree :: Map -> Int -> Int -> Bool
isTree map x y = ((map !! y) !! x_mod) == '#' where x_mod = x `mod` length (head map)

getTreesSlopeRecursive :: Map -> Int -> Int -> Int -> Int -> Int -> Int
getTreesSlopeRecursive worldMap xStep yStep x y acc
    | y >= yLimit         = acc
    | isTree worldMap x y = getTreesSlopeRecursive worldMap xStep yStep (x + xStep) (y + yStep) (acc + 1)
    | otherwise           = getTreesSlopeRecursive worldMap xStep yStep (x + xStep) (y + yStep) acc
    where yLimit = length worldMap

getTreesSlope :: Map -> Int -> Int -> Int
getTreesSlope worldMap xStep yStep = getTreesSlopeRecursive worldMap xStep yStep 0 0 0

dayThree :: IO ()
dayThree = do
    input_Text <- fmap Text.lines (Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_three.txt")
    let input_strs = map Text.unpack input_Text
    -- part 1
    putStr "The answer for day three part one is "
    print $ getTreesSlope input_strs 3 1
    -- part 2 
    putStr "The answer for day three part two is "
    print $ product $ [ getTreesSlope input_strs x 1 | x <- [1, 3 .. 7] ] ++ [getTreesSlope input_strs 1 2]

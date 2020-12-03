module DayOne
    ( dayOne
    )
where

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

dayOne :: IO ()
dayOne = do
    input_Text <- Text.lines <$> Text.readFile "/home/jamie/Git/AdventOfCode/2020/Inputs/day_one.txt"
    let input_strs = map Text.unpack input_Text
    let input_ints = map read input_strs
    -- part 1
    putStr "The answer for day one part one is "
    print $ head [ x * y | x <- input_ints, y <- input_ints, x + y == 2020 ]
    -- part 2
    putStr "The answer for day one part two is "
    print $ head [ x * y * z | x <- input_ints, y <- input_ints, z <- input_ints, x + y + z == 2020 ]

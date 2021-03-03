module Common
    ( split
    , countCharString
    , groupEntries
    )
where


split :: Eq a => a -> [a] -> [[a]]
split _      []      = []
split onChar toSplit = x : split onChar (drop 1 y)
    where (x, y) = span (/= onChar) toSplit

countCharString :: String -> Char -> Int
countCharString str c = length $ filter (== c) str

groupEntries :: [String] -> [String]
groupEntries strings
    | null strings = []
    | otherwise = unwords pass : groupEntries (drop (length pass + 1) strings)
    where pass = takeWhile (not . null) strings

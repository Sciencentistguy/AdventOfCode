module Common
    ( split
    )
where


split :: Eq a => a -> [a] -> [[a]]
split _      []      = []
split onChar toSplit = x : split onChar (drop 1 y) where (x, y) = span (/= onChar) toSplit

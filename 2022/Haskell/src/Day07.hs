module Day07 (day07) where

import AoC hiding (Token)
import Common
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text

type Parsed = File

data File = Dir String [File] | File String Int deriving (Show)

getFilesystem :: [String] -> File -> ([String], File)
getFilesystem [] fs = ([], fs)
getFilesystem (cmd : cmds) (Dir name files)
  | cmd == "$ cd .." = (cmds, Dir name files)
  | "dir" `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
  | "$ ls" `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
  | "$ cd" `isPrefixOf` cmd = getFilesystem ncmds (Dir name (sfs : files))
  | otherwise = getFilesystem cmds (Dir name (nf : files))
  where
    (size, fname) = case words cmd of
      [size, fname] -> (size, fname)
      _ -> undefined
    nf = File fname (read size)
    (ncmds, sfs) = getFilesystem cmds (Dir (drop 5 cmd) [])
getFilesystem _ _ = undefined

getSize :: File -> Int
getSize (File _ size) = size
getSize (Dir _ files) = sum . fmap getSize $ files

getDirSizes :: File -> [Int]
getDirSizes (File _ _) = []
getDirSizes (Dir n files) =
  getSize (Dir n files) : foldl (\a d -> a ++ getDirSizes d) [] files

day07 :: Runner Parsed Int
day07 =
  let year = 2022
      day = 7
      parser :: Text -> Maybe Parsed
      parser = return . snd . flip getFilesystem (Dir "/" []) . drop 2 . lines . Text.unpack
      part1 :: Parsed -> Maybe Int
      part1 = return . sum . filter (<= 100000) . getDirSizes
      part2 input =
        let dirSizes = getDirSizes input
            unused = 70000000 - getSize input
         in return . minimum . filter (>= (30000000 - unused)) $ dirSizes
   in Runner {..}

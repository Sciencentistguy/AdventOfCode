module Common where

import Control.Monad.Except

type IOResult e = ExceptT e IO

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split onChar toSplit = before : split onChar (drop 1 after)
  where
    (before, after) = span (/= onChar) toSplit

unwrap :: (Monad m, Show a) => ExceptT a m b -> m b
unwrap res =
  runExceptT res >>= \case
    Right a -> return a
    Left err -> error $ show err

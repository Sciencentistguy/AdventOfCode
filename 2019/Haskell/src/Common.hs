module Common
  ( module Common,
    module Control.Monad.Except,
  )
where

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

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe _ (Just a) = return a
liftMaybe err Nothing = throwError err

module Common
  ( module Common,
    module Control.Monad.Except,
  )
where

import Control.Monad.Except
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type IOResult e = ExceptT e IO

type Parser = Parsec Void String

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

spaces :: Parser ()
spaces =
  L.space
    space1
    (return ())
    (return ())

symbol :: String -> Parser String
symbol = L.symbol spaces

parseUnwrap ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Either (ParseErrorBundle s e) p ->
  p
parseUnwrap (Right a) = a
parseUnwrap (Left peb) = error $ errorBundlePretty peb

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

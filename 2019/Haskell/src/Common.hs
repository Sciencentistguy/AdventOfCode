module Common
  ( module Common,
    module Control.Monad.Except,
  )
where

import Control.Monad.Except
import Control.Monad.ST
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type STResult s e = ExceptT e (ST s)

type Parser = Parsec Void Text

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

symbol :: Text -> Parser Text
symbol = L.symbol spaces

parseUnwrap ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Either (ParseErrorBundle s e) p ->
  p
parseUnwrap (Right a) = a
parseUnwrap (Left peb) = error $ errorBundlePretty peb

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

unwrapParser ::
  (Monad m, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Either (ParseErrorBundle s e) a ->
  m a
unwrapParser p = case p of
  Right x -> return x
  Left e -> error $ errorBundlePretty e

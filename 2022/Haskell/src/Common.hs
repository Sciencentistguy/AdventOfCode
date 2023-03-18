module Common where

import GHC.IO (unsafePerformIO)
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, TraversableStream, VisualStream, errorBundlePretty)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split onChar toSplit = before : split onChar (drop 1 after)
  where
    (before, after) = span (/= onChar) toSplit

unwrapPEB ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Either (ParseErrorBundle s e) a ->
  Maybe a
unwrapPEB (Left e) = unsafePerformIO $ do
  putStrLn $ errorBundlePretty e
  return Nothing
unwrapPEB (Right x) = Just x
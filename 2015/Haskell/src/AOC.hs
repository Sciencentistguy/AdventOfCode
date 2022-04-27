{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Network.HTTP.Req
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
    doesFileExist,
  )
import System.Environment (getEnv)
import System.Posix.ByteString (CMode (CMode))
import Text.Printf (printf)

newtype Token = Token Text

data Runner a out = Runner
  { day :: Int,
    year :: Int,
    parser :: Text -> Maybe a,
    part1 :: a -> Maybe out,
    part2 :: a -> Maybe out
  }

runAoC :: Show out => Token -> Runner a out -> IO ()
runAoC token Runner {..} = do
  ensureCacheExists year
  input <- fetchInput token year day
  let parsed = fromJust $ parser input
      part1Solution = fromJust $ part1 parsed
  putStrLn $ printf "The solution to %d day %02d (part 1) is: %s" year day (show part1Solution)
  let part2Solution = fromJust $ part2 parsed
  putStrLn $ printf "The solution to %d day %02d (part 2) is: %s" year day (show part2Solution)
  putStrLn ""

fetchInput :: Token -> Int -> Int -> IO Text
fetchInput token year day =
  cacheRead year day >>= \case
    Just entry -> return entry
    Nothing -> do
      fetched <- fetchFromAPI token year day
      cacheWrite year day fetched
      return fetched

fetchFromAPI :: Token -> Int -> Int -> IO Text
fetchFromAPI (Token token) year day = do
  let cookie = printf "session=%s" token :: String
  let url =
        https "adventofcode.com"
          /: textShow year
          /: "day"
          /: textShow day
          /: "input"
  r <-
    runReq defaultHttpConfig $
      req
        GET
        url
        NoReqBody
        bsResponse
        (header (fromString "COOKIE") (fromString cookie))

  return $ Text.decodeUtf8 $ responseBody r

ensureCacheExists :: Int -> IO ()
ensureCacheExists year = do
  home <- getEnv "HOME"
  let cache_path = printf "%s/.aoc" home
  exists <- doesDirectoryExist cache_path
  unless exists $ createDirectory cache_path
  let year_path = printf "%s/%d" cache_path year
  exists <- doesDirectoryExist year_path
  unless exists $ createDirectory year_path

cacheRead :: Int -> Int -> IO (Maybe Text)
cacheRead year day = do
  home <- getEnv "HOME"
  let path = printf "%s/.aoc/%d/day%02d.txt" home year day
  entryExists <- doesFileExist path
  if entryExists
    then return <$> Text.readFile path
    else return Nothing

cacheWrite :: Int -> Int -> Text -> IO ()
cacheWrite year day text = do
  home <- getEnv "HOME"
  let path = printf "%s/.aoc/%d/day%02d.txt" home year day
  Text.writeFile path text

textShow :: Show a => a -> Text
textShow = Text.pack . show

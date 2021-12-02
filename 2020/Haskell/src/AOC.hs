{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC where

import Control.Monad
import Data.Maybe (fromJust)
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import Network.HTTP.Req
import System.Directory
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
runAoC (Token token) runner@Runner {..} = do
  cacheInit year
  input <- fetchCached runner token
  let parsed = fromJust $ parser input
  let part1Solution = fromJust $ part1 parsed
  putStrLn $ printf "The solution to %d day %02d (part 1) is: %s" year day (show part1Solution)
  let part2Solution = fromJust $ part2 parsed
  putStrLn $ printf "The solution to %d day %02d (part 2) is: %s" year day (show part2Solution)
  putStrLn ""

fetchCached :: Runner a out -> Text -> IO Text
fetchCached runner@Runner {day, year} token = do
  cacheEntry <- cacheRead year day
  case cacheEntry of
    Just entry -> return entry
    Nothing -> do
      fetched <- fetchRaw runner token
      cacheWrite year day fetched
      return fetched

fetchRaw :: Runner a out -> Text -> IO Text
fetchRaw Runner {day, year} token = do
  let cookie = printf "session=%s" token :: String
  let url =
        https "adventofcode.com"
          /: fromString (show year)
          /: "day"
          /: fromString (show day)
          /: "input"

  r <-
    runReq defaultHttpConfig $
      req
        GET
        url
        NoReqBody
        bsResponse
        (header (fromString "COOKIE") (fromString cookie))

  return $ decodeUtf8 $ responseBody r

cacheInit :: Int -> IO ()
cacheInit year = do
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
  entryExists <- doesDirectoryExist path
  if entryExists
    then return <$> Text.readFile path
    else return Nothing

cacheWrite :: Int -> Int -> Text -> IO ()
cacheWrite year day text = do
  home <- getEnv "HOME"
  let path = printf "%s/.aoc/%d/day%02d.txt" home year day
  Text.writeFile path text

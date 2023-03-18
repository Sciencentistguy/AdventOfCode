{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AoC where

import Control.Monad (unless)
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
import System.Environment (getEnv, lookupEnv)
import Text.Printf (printf)
import System.IO
import System.Exit

newtype Token = Token Text

getTokenFromEnv :: IO (Maybe Token)
getTokenFromEnv = fmap (Token . Text.pack) <$> lookupEnv "TOKEN"

getTokenFromFile :: String -> IO Token
getTokenFromFile filename = Token . Text.pack <$> readFile filename

getToken :: IO Token
getToken =
  getTokenFromEnv >>= \case
    Just token -> return token
    Nothing -> getTokenFromFile "tokenfile"

type Solver a out = a -> Maybe out

data Runner a out = Runner
  { day :: Int,
    year :: Int,
    parser :: Text -> Maybe a,
    part1 :: Solver a out,
    part2 :: Solver a out
  }

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

runAoC :: Show out => Token -> Runner a out -> IO ()
runAoC token Runner {..} = do
  ensureCacheExists year
  input <- fetchInput token year day
  putStrLn $ printf "%d day %02d" year day
  parsed <- case parser input of
    Nothing -> ePutStrLn "Failed to parse input" >> exitFailure
    Just x -> return x
  part1Solution <- case part1 parsed of
    Nothing -> ePutStrLn "Failed to solve part 1" >> exitFailure
    Just x -> return x
  putStrLn $ printf "Part 1: %s" (show part1Solution)
  part2Solution <- case part2 parsed of
    Nothing -> ePutStrLn "Failed to solve part 2" >> exitFailure
    Just x -> return x
  putStrLn $ printf "Part 2: %s" (show part2Solution)

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

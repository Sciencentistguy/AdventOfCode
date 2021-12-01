{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC where

import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req
import Text.Printf (printf)

newtype Token = Token Text

data Runner a = Runner
  { day :: Int,
    year :: Int,
    parser :: Text -> Maybe a,
    part1 :: a -> Maybe Integer,
    part2 :: a -> Maybe Integer
  }

runAoC :: Token -> Runner a -> IO ()
runAoC (Token token) runner@Runner {..} = do
  input <- fetchRaw runner token
  let parsed = fromJust $ parser input
  let part1Solution = fromJust $ part1 parsed
  putStrLn $ printf "The solution to %d day %02d (part 1) is: %d" year day part1Solution
  let part2Solution = fromJust $ part2 parsed
  putStrLn $ printf "The solution to %d day %02d (part 2) is: %d" year day part2Solution
  putStrLn ""

fetchRaw :: Runner a -> Text -> IO Text
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

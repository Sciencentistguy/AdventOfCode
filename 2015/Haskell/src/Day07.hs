{-# LANGUAGE OverloadedStrings #-}

module Day07
  ( day07,
  )
where

import AoC (Runner (..))
import Common
import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Char (isAlpha, isDigit)
import Data.Functor (($>), (<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text

-- import Data.Word (Int)

data Name
  = ConstName Int
  | ConnectedName String

data Connection
  = ConstConnection Int
  | UnaryConnection (Int -> Int) Name
  | BinaryConnection (Int -> Int -> Int) Name Name

type Circuit = HashMap String Connection

parseName :: String -> Name
parseName xs@(x : _)
  | isDigit x = ConstName (read xs)
  | isAlpha x = ConnectedName xs
  | otherwise = error "malformed name"
parseName _ = error "empty name"

parseGate :: [String] -> (String, Connection)
parseGate [a, "->", r] =
  (r, UnaryConnection id (parseName a))
parseGate ["NOT", a, "->", r] =
  (r, UnaryConnection complement (parseName a))
parseGate [a1, "LSHIFT", a2, "->", r] =
  (r, BinaryConnection shiftL (parseName a1) (parseName a2))
parseGate [a1, "RSHIFT", a2, "->", r] =
  (r, BinaryConnection shiftR (parseName a1) (parseName a2))
parseGate [a1, "AND", a2, "->", r] =
  (r, BinaryConnection (.&.) (parseName a1) (parseName a2))
parseGate [a1, "OR", a2, "->", r] =
  (r, BinaryConnection (.|.) (parseName a1) (parseName a2))
parseGate _ = error "bad gate"

-- | Parse the list of gates.
parseGates :: String -> Circuit
parseGates stuff = HM.fromList $ parseGate . words <$> lines stuff

-- | Evaluate a single 'Term' in the context of
-- a 'GateMap', returning a new 'GateMap' with
-- the term substituted for its value, together
-- with the value.
evalTerm :: Circuit -> Name -> (Circuit, Int)
evalTerm circuit (ConstName n) =
  (circuit, n)
evalTerm circuit (ConnectedName a) =
  let (circuit', n) = eval circuit a
   in (HM.insert a (ConstConnection n) circuit', n)

-- | Evaluate a variable in the context of
-- a 'GateMap', returning the result of the
-- evaluation plus a new 'GateMap' with
-- all substitutions made.
eval :: Circuit -> String -> (Circuit, Int)
eval circuit target =
  case HM.lookup target circuit of
    Just (ConstConnection n) ->
      (circuit, n)
    Just (UnaryConnection op a) ->
      let (circuit', n) = evalTerm circuit a
       in (circuit', op n)
    Just (BinaryConnection op a1 a2) ->
      let (circuit', n1) = evalTerm circuit a1
       in let (circuit'', n2) = evalTerm circuit' a2
           in (circuit'', op n1 n2)
    Nothing -> error "bad target"

type Parsed = Circuit

day07 :: Runner Parsed Int
day07 =
  let year = 2015
      day = 7
      parser :: Text -> Maybe Parsed
      parser = return . parseGates . Text.unpack
      part1 circuit = return $ snd $ eval circuit "a"
      part2 circuit =
        let n = snd $ eval circuit "a"
            circuit' = HM.insert "b" (ConstConnection n) circuit
         in return $ snd $ eval circuit' "a"
   in Runner {..}

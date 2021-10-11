module Intcode where

import Common
import Control.Monad.Primitive
import Data.IORef
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Debug.Trace (trace)
import Safe (headMay)

type Memory = V.MVector (PrimState IO) Int

type Arg = (Int, AddressingMode)

data Instruction
  = Add Arg Arg Arg
  | Multiply Arg Arg Arg
  | Input Arg
  | Output Arg
  | JumpIfTrue Arg Arg
  | JumpIfFalse Arg Arg
  | LessThan Arg Arg Arg
  | Equals Arg Arg Arg
  | Halt

instructionLength :: Instruction -> Int
instructionLength Add {} = 4
instructionLength Multiply {} = 4
instructionLength Input {} = 2
instructionLength Output {} = 2
instructionLength Halt = 1
instructionLength JumpIfTrue {} = 3
instructionLength JumpIfFalse {} = 3
instructionLength LessThan {} = 4
instructionLength Equals {} = 4

data IntcodeComputer = IntcodeComputer
  { memory :: Memory,
    pcPtr :: IORef Int,
    inputsPtr :: IORef [Int],
    outputsPtr :: IORef [Int],
    debug :: Bool
  }

data IntcodeError
  = MemoryOutOfBoundsError Int Int
  | InvalidOpcodeError Int
  | NoInputError

data AddressingMode = Immediate | Position deriving (Show, Eq)

pMode :: Int -> AddressingMode
pMode 0 = Position
pMode 1 = Immediate
pMode x = error $ "Invalid addressing mode: " ++ show x

instance Show IntcodeError where
  show (MemoryOutOfBoundsError addr max) =
    "Error: out of bounds memory index: accessed `"
      ++ show addr
      ++ "`, memory len `"
      ++ show max
      ++ "`"
  show (InvalidOpcodeError opcode) =
    "Error: invalid opcode `"
      ++ show opcode
      ++ "`"
  show NoInputError = "Error: No input available"

initIntcode :: [Int] -> IO IntcodeComputer
initIntcode source = do
  pcPtr <- newIORef 0
  memory <- V.thaw (V.fromList source)
  inputsPtr <- newIORef []
  outputsPtr <- newIORef []
  let debug = False
  return IntcodeComputer {..}

setInput :: [Int] -> IntcodeComputer -> IO IntcodeComputer
setInput ls pc@IntcodeComputer {..} = writeIORef inputsPtr ls >> return pc

setDebug :: Monad m => IntcodeComputer -> m IntcodeComputer
setDebug IntcodeComputer {..} = do
  let debug = True
  return IntcodeComputer {..}

data Status = Finished | NotFinished

runInstruction :: IntcodeComputer -> IOResult IntcodeError Status
runInstruction IntcodeComputer {..} = do
  pc <- liftIO $ readIORef pcPtr
  instr <- do
    rawOpcode <- readPosition pc
    let opcode = rawOpcode `mod` 100
    let mode1 = pMode $ rawOpcode `mod` 1000 `div` 100
    let mode2 = pMode $ rawOpcode `mod` 10000 `div` 1000
    let mode3 = pMode $ rawOpcode `mod` 100000 `div` 10000
    case opcode of
      1 -> do
        arg1 <- readPosition $ pc + 1
        arg2 <- readPosition $ pc + 2
        arg3 <- readPosition $ pc + 3
        return $ Add (arg1, mode1) (arg2, mode2) (arg3, mode3)
      2 -> do
        arg1 <- readPosition $ pc + 1
        arg2 <- readPosition $ pc + 2
        arg3 <- readPosition $ pc + 3
        return $ Multiply (arg1, mode1) (arg2, mode2) (arg3, mode3)
      3 -> do
        arg <- readPosition $ pc + 1
        return $ Input (arg, mode1)
      4 -> do
        arg <- readPosition $ pc + 1
        return $ Output (arg, mode1)
      5 -> do
        arg1 <- readPosition $ pc + 1
        arg2 <- readPosition $ pc + 2
        return $ JumpIfTrue (arg1, mode1) (arg2, mode2)
      6 -> do
        arg1 <- readPosition $ pc + 1
        arg2 <- readPosition $ pc + 2
        return $ JumpIfFalse (arg1, mode1) (arg2, mode2)
      7 -> do
        arg1 <- readPosition $ pc + 1
        arg2 <- readPosition $ pc + 2
        arg3 <- readPosition $ pc + 3
        return $ LessThan (arg1, mode1) (arg2, mode2) (arg3, mode3)
      8 -> do
        arg1 <- readPosition $ pc + 1
        arg2 <- readPosition $ pc + 2
        arg3 <- readPosition $ pc + 3
        return $ Equals (arg1, mode1) (arg2, mode2) (arg3, mode3)
      99 -> return Halt
      x -> throwError $ InvalidOpcodeError x

  when debug $ liftIO do
    x <- readIORef pcPtr
    putStrLn $ "pc = " ++ show x

  debugInstr instr

  status <- case instr of
    Halt -> return Finished
    Add (arg1, mode1) (arg2, mode2) (arg3, _) -> do
      result <- liftM2 (+) (address mode1 arg1) (address mode2 arg2)
      writePosition arg3 result
      liftIO $ modifyIORef' pcPtr (+ instructionLength instr)
      return NotFinished
    Multiply (arg1, mode1) (arg2, mode2) (arg3, _) -> do
      result <- liftM2 (*) (address mode1 arg1) (address mode2 arg2)
      writePosition arg3 result
      liftIO $ modifyIORef' pcPtr (+ instructionLength instr)
      return NotFinished
    Input (arg, _) -> do
      response <- popInput
      writePosition arg response
      liftIO $ modifyIORef' pcPtr (+ instructionLength instr)
      return NotFinished
    Output (arg, mode) -> do
      val <- address mode arg
      pushOutput val
      liftIO $ modifyIORef' pcPtr (+ instructionLength instr)
      return NotFinished
    JumpIfTrue (arg1, mode1) (arg2, mode2) -> do
      val1 <- address mode1 arg1
      if isTrue val1
        then address mode2 arg2 >>= setPC
        else liftIO $ modifyIORef' pcPtr (+ instructionLength instr)
      return NotFinished
    JumpIfFalse (arg1, mode1) (arg2, mode2) -> do
      val <- address mode1 arg1
      if not $ isTrue val
        then address mode2 arg2 >>= setPC
        else liftIO $ modifyIORef' pcPtr (+ instructionLength instr)
      return NotFinished
    LessThan (arg1, mode1) (arg2, mode2) (arg3, _) -> do
      cond <- liftM2 (<) (address mode1 arg1) (address mode2 arg2)
      if cond
        then writePosition arg3 1
        else writePosition arg3 0
      liftIO $ modifyIORef' pcPtr (+ instructionLength instr)
      return NotFinished
    Equals (arg1, mode1) (arg2, mode2) (arg3, _) -> do
      cond <- liftM2 (==) (address mode1 arg1) (address mode2 arg2)
      if cond
        then writePosition arg3 1
        else writePosition arg3 0
      liftIO $ modifyIORef' pcPtr (+ instructionLength instr)
      return NotFinished

  when debug $ liftIO $ putStrLn ""
  return status
  where
    address :: AddressingMode -> Int -> IOResult IntcodeError Int
    address mode arg = case mode of
      Position -> do
        val <- readPosition arg
        when debug $ liftIO do
          putStr "read at "
          putStr $ show arg
          putStr ": "
          print val
        return val
      Immediate -> return arg

    readPosition :: Int -> IOResult IntcodeError Int
    readPosition pos =
      if VM.length memory < pos
        then --then throwError $ MemoryOutOfBoundsError pos (VM.length memory)
          error "in readPosition"
        else VM.read memory pos

    writePosition :: Int -> Int -> IOResult IntcodeError ()
    writePosition pos val = do
      if VM.length memory < pos
        then throwError $ MemoryOutOfBoundsError pos (VM.length memory)
        else do
          when debug $ liftIO do
            a <- VM.read memory pos
            putStr "write at "
            putStr $ show pos
            putStr ": "
            putStr $ show a
            putStr " -> "
            print val
          VM.write memory pos val

    setPC :: Int -> IOResult IntcodeError ()
    setPC newVal = do
      when debug $ liftIO do
        putStr "pc "
        readIORef pcPtr >>= putStr . show
        putStr " -> "
        print newVal
      liftIO $ writeIORef pcPtr newVal

    popInput :: IOResult IntcodeError Int
    popInput = do
      inputs <- liftIO $ readIORef inputsPtr
      if null inputs
        then throwError NoInputError
        else do
          let (x : inputs') = inputs
          liftIO $ writeIORef inputsPtr inputs'
          return x

    pushOutput :: Int -> IOResult IntcodeError ()
    pushOutput x = liftIO $ modifyIORef' outputsPtr (++ [x])

    debugInstr :: Instruction -> IOResult IntcodeError ()
    debugInstr instr = when debug do
      case instr of
        Halt -> liftIO $ putStr "hlt"
        Add (arg1, mode1) (arg2, mode2) (arg3, mode3) -> liftIO do
          putStr "add  "
          when (mode1 == Immediate) $ putStr "*"
          putStr $ show arg1
          putStr " "
          when (mode2 == Immediate) $ putStr "*"
          putStr $ show arg2
          putStr " "
          when (mode3 == Immediate) $ putStr "*"
          putStr $ show arg3
        Multiply (arg1, mode1) (arg2, mode2) (arg3, mode3) -> liftIO do
          putStr "mul  "
          when (mode1 == Immediate) $ putStr "*"
          putStr $ show arg1
          putStr " "
          when (mode2 == Immediate) $ putStr "*"
          putStr $ show arg2
          putStr " "
          when (mode3 == Immediate) $ putStr "*"
          putStr $ show arg3
        Input (arg, _) -> liftIO do
          putStr "in   "
          putStr $ show arg
        Output (arg, mode) -> liftIO do
          putStr "out  "
          when (mode == Immediate) $ putStr "*"
          putStr $ show arg
        JumpIfTrue (arg1, mode1) (arg2, mode2) -> liftIO do
          putStr "jit  "
          when (mode1 == Immediate) $ putStr "*"
          putStr $ show arg1
          putStr " "
          when (mode2 == Immediate) $ putStr "*"
          putStr $ show arg2
        JumpIfFalse (arg1, mode1) (arg2, mode2) -> liftIO do
          putStr "jif  "
          when (mode1 == Immediate) $ putStr "*"
          putStr $ show arg1
          putStr " "
          when (mode2 == Immediate) $ putStr "*"
          putStr $ show arg2
        LessThan (arg1, mode1) (arg2, mode2) (arg3, mode3) -> liftIO do
          putStr "lt   "
          when (mode1 == Immediate) $ putStr "*"
          putStr $ show arg1
          putStr " "
          when (mode2 == Immediate) $ putStr "*"
          putStr $ show arg2
          putStr " "
          when (mode3 == Immediate) $ putStr "*"
          putStr $ show arg3
        Equals (arg1, mode1) (arg2, mode2) (arg3, mode3) -> liftIO do
          putStr "eq   "
          when (mode1 == Immediate) $ putStr "*"
          putStr $ show arg1
          putStr " "
          when (mode2 == Immediate) $ putStr "*"
          putStr $ show arg2
          putStr " "
          when (mode3 == Immediate) $ putStr "*"
          putStr $ show arg3

      liftIO $ putStrLn ""

data FinishedComputer = FinishedComputer
  { memory' :: V.Vector Int,
    output :: V.Vector Int
  }
  deriving (Show)

finish :: IntcodeComputer -> IO FinishedComputer
finish IntcodeComputer {..} = do
  memory' <- V.freeze memory
  output <- V.fromList <$> readIORef outputsPtr
  return FinishedComputer {..}

runIntcode :: IntcodeComputer -> IOResult IntcodeError FinishedComputer
runIntcode computer = do
  status <- runInstruction computer
  case status of
    -- TODO This can probably be a V.unsafeFreeze
    Finished -> liftIO $ finish computer
    NotFinished -> runIntcode computer

isTrue :: Int -> Bool
isTrue 0 = False
isTrue _ = True

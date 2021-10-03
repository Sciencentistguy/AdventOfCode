module Intcode where

import Common
import Control.Monad.Primitive
import Data.IORef
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

type Memory = V.MVector (PrimState IO) Int

data IntcodeComputer = IntcodeComputer
  { memory :: Memory,
    pcPtr :: IORef Int
  }

data IntcodeError
  = MemoryOutOfBoundsError Int Int
  | InvalidOpcodeError Int

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

initIntcode :: [Int] -> IO IntcodeComputer
initIntcode source = do
  pc <- newIORef 0
  (`IntcodeComputer` pc) <$> V.thaw (V.fromList source)

data Status = Finished | NotFinished

runInstruction :: IntcodeComputer -> IOResult IntcodeError Status
runInstruction IntcodeComputer {..} = do
  pc <- liftIO $ readIORef pcPtr
  opcode <- readPosition pc

  case opcode of
    1 -> do
      arg1 <- readPosition (pc + 1)
      arg2 <- readPosition (pc + 2)
      arg3 <- readPosition (pc + 3)
      result <-
        liftM2
          (+)
          (readPosition arg1)
          (readPosition arg2)
      writePosition arg3 result
      liftIO $ modifyIORef' pcPtr (+ 4)
      return NotFinished
    2 -> do
      arg1 <- readPosition (pc + 1)
      arg2 <- readPosition (pc + 2)
      arg3 <- readPosition (pc + 3)
      result <-
        liftM2
          (*)
          (readPosition arg1)
          (readPosition arg2)
      writePosition arg3 result
      liftIO $ modifyIORef' pcPtr (+ 4)
      return NotFinished
    99 -> return Finished
    x -> throwError $ InvalidOpcodeError x
  where
    readPosition :: Int -> IOResult IntcodeError Int
    readPosition pos =
      if VM.length memory < pos
        then throwError $ MemoryOutOfBoundsError pos (VM.length memory)
        else VM.read memory pos
    writePosition :: Int -> Int -> IOResult IntcodeError ()
    writePosition pos val =
      if VM.length memory < pos
        then throwError $ MemoryOutOfBoundsError pos (VM.length memory)
        else VM.write memory pos val

runIntcode :: IntcodeComputer -> IOResult IntcodeError (V.Vector Int)
runIntcode computer = do
  status <- runInstruction computer
  case status of
    -- TODO This can probably be a V.unsafeFreeze
    Finished -> V.freeze $ memory computer
    NotFinished -> runIntcode computer

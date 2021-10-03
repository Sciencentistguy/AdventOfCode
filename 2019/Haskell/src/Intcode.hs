module Intcode where

import Common
import Control.Monad.Except
import Control.Monad.Primitive
import Data.IORef
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

type Memory = V.MVector (PrimState IO) Int

data IntcodeComputer = IntcodeComputer
  { instructions :: Memory,
    pcPtr :: IORef Int
  }

initIntcode :: [Int] -> IO IntcodeComputer
initIntcode source = do
  pc <- newIORef 0
  (`IntcodeComputer` pc) <$> V.thaw (V.fromList source)

data Status = Finished | NotFinished

runInstruction :: IntcodeComputer -> IOResult () Status
runInstruction IntcodeComputer {..} = do
  pc <- liftIO $ readIORef pcPtr
  x <- V.freeze $ VM.slice pc 4 instructions
  let [opcode, arg1, arg2, arg3] = V.toList x
  case opcode of
    1 -> do
      result <-
        liftM2
          (+)
          (readPosition arg1)
          (readPosition arg2)
      writePosition arg3 result
      liftIO $ modifyIORef' pcPtr (+ 4)
      return NotFinished
    2 -> do
      result <-
        liftM2
          (*)
          (readPosition arg1)
          (readPosition arg2)
      writePosition arg3 result
      liftIO $ modifyIORef' pcPtr (+ 4)
      return NotFinished
    99 -> return Finished
    x -> error $ "invalid opcode: " ++ show x
  where
    readPosition :: Int -> IOResult () Int
    readPosition pos =
      if VM.length instructions < pos
        then throwError ()
        else VM.read instructions pos
    writePosition :: Int -> Int -> IOResult () ()
    writePosition pos val =
      if VM.length instructions < pos
        then throwError ()
        else VM.write instructions pos val

runIntcode :: IntcodeComputer -> IOResult () (V.Vector Int)
runIntcode computer = do
  status <- runInstruction computer
  case status of
    Finished -> V.freeze $ instructions computer
    NotFinished -> runIntcode computer

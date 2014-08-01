module Main where

import Bytecode.Function
import Bytecode.Ops
import Representation.Cell
import ObjectCode.Persistent
import Data.Char (ord)
import Data.Binary (encodeFile)
import qualified Data.Map as M
import qualified Data.Vector as V


sample :: FunctionSpace PIOp
sample = M.fromList [("builtin$main", Function 8 mainFun), ("user$newline", Function 8 printNewline)]
    where mainFun = V.fromList [MemAlloc 8, SetAFP, load 80, WriteValue DWord, Call "user$newline", Exit]
          printNewline = V.fromList [loadChar '\n', WriteChar, Return]

load :: Int -> GenOp gptr lptr label
load = LoadDWord . fromIntegral

loadChar :: Char -> GenOp gptr lptr label
loadChar = LoadByte . fromIntegral . ord

initialHeap :: M.Map String (V.Vector Cell)
initialHeap = M.empty

machine :: PersistentMachine
machine = PersistentMachine initialHeap sample

main :: IO ()
main = encodeFile "init.jcb" machine

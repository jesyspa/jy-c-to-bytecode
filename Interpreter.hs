module Main where

import Bytecode.Ops
import Interpreter.Interpret (interpret)
import Interpreter.Machine
import Data.Char (ord)
import qualified Data.Map as M
import qualified Data.Vector as V

helloWorld :: FunctionSpace
helloWorld = M.fromList [("main", Function 8 mainFun), ("newline", Function 8 printNewline)]
    where mainFun = V.fromList $ [MemAlloc 8, SetAFP, load 80, WriteValue DWord, Call "newline", Exit]
          printNewline = V.fromList $ [loadChar '\n', WriteChar, Return]

load :: Int -> Op
load = LoadDWord . fromIntegral

loadChar :: Char -> Op
loadChar = LoadByte . fromIntegral . ord

initialMachine :: Machine
initialMachine = Machine ("main", 0) 0 M.empty []

main :: IO ()
main = do
    putStrLn "Running interpreter..."
    interpret helloWorld initialMachine
    putStrLn "Done!"

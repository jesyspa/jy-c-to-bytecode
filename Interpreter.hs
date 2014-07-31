module Main where

import Bytecode.Ops
import Bytecode.Representable
import Interpreter.Machine
import Interpreter.Interpret (interpret)
import qualified Data.Map as M
import qualified Data.Vector as V

helloWorld :: FunctionSpace
helloWorld = M.singleton "main" $ Function 0 helloCode
    where helloCode = V.fromList $ [load 5, load 6, Add DWord, WriteValue DWord, loadChar '\n', WriteChar, Exit]

load :: Int -> Op
load = LoadImmediate . toRepresentation

loadChar :: Char -> Op
loadChar = LoadImmediate . toRepresentation

initialMachine :: Machine
initialMachine = Machine ("main", 0) 0 M.empty []

main :: IO ()
main = do
    putStrLn "Running interpreter..."
    interpret helloWorld initialMachine
    putStrLn "Done!"

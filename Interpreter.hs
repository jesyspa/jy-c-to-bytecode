module Main where

import Bytecode.Ops
import Interpreter.Machine
import Interpreter.Interpret (interpret)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Char (ord)

helloWorld :: FunctionSpace
helloWorld = M.singleton "main" $ Function 0 helloCode
    where helloCode = V.fromList $ concatMap (\x -> [LoadImmediate Byte $ ord x, WriteChar]) "Hello world!\n" ++ [Exit]

initialMachine :: Machine
initialMachine = Machine ("main", 0) 0 M.empty []

main :: IO ()
main = do
    putStrLn "Running interpreter..."
    interpret helloWorld initialMachine
    putStrLn "Done!"

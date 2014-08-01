module Main where

import Interpreter.Interpret (interpret)
import Interpreter.Machine
import Data.Binary (decodeFile)

main :: IO ()
main = do
    putStrLn "Running interpreter..."
    pm <- decodeFile "init.jcb"
    (m, fs)  <- fromPersistent pm
    interpret fs m
    putStrLn "Done!"

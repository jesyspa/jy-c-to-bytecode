module Interpreter.Debug (
    dumpStack
) where

import Interpreter.Cell
import Interpreter.Machine
import Control.Lens
import Control.Monad.State

printCell :: (MonadIO m) => Cell -> m ()
printCell (ByteCell PtrStart x) = liftIO $ putStrLn $ "[p] " ++ show x
printCell (ByteCell None x) = liftIO $ putStrLn $ "    " ++ show x
printCell (NameCell x) = liftIO $ putStrLn $ "[f] " ++ x

dumpStack :: (MonadIO m) => Machine -> m ()
dumpStack machine = do
    liftIO $ putStrLn "\n--- Stack dump ---"
    mapM_ printCell $ machine^.stack
    liftIO $ putStrLn "---  End dump  ---"


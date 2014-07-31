module Interpreter.Interpret (
    interpret
) where

import Bytecode.Ops
import Interpreter.Cell
import Interpreter.Machine
import Interpreter.Error
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Data.Vector
import Data.Char (chr)

interpret :: FunctionSpace -> Machine -> IO ()
interpret funs machine = result >>= \x -> case x of
                                              ExitSuccess -> return ()
                                              err -> putStr "Error: " >> print err
    where steps = forever step
          -- This unconditional match is correct here.  We know that if a Right occured, the next step would be
          -- performed, meaning that if the computation completed, we have a Left.
          result = fmap (\(Left x) -> x) . runExceptT $ steps `runReaderT` funs `evalStateT` machine

type MonadStack m = (MonadReader FunctionSpace m, MonadState Machine m, MonadError InterpError m, MonadIO m)

fromMaybeOr :: (MonadError e m) => Maybe a -> e -> m a
fromMaybeOr v e = maybe (throwError e) return v

infix 0 `useOr`
useOr :: (MonadError e m, MonadState s m) => Getting (Maybe a) s (Maybe a) -> e -> m a
useOr g e = use g >>= maybe (throwError e) return

infix 0 `viewOr`
viewOr :: (MonadError e m, MonadReader s m) => Getting (Maybe a) s (Maybe a) -> e -> m a
viewOr g e = view g >>= maybe (throwError e) return


step :: MonadStack m => m ()
step = do
    (funname, offset) <- use ip
    fun <- at funname . (to . fmap . view) code `viewOr` InvalidFunction
    opcode <- fun !? offset `fromMaybeOr` CodeOutOfBounds
    ip._2 += 1
    exec opcode
    --undefined

popStack :: MonadStack m => m Cell
popStack = stack . to (^?_head) `useOr` StackUnderflow

pushStack :: MonadStack m => Cell -> m ()
pushStack v = stack %= (v:)

cellAsChar :: Cell -> Char
cellAsChar (ByteCell _ x) = chr $ fromIntegral x
cellAsChar (NameCell _) = '\0'

exec :: MonadStack m => Op -> m ()
exec (LoadImmediate Byte x) = pushStack $ ByteCell None (fromIntegral x)
exec WriteChar = popStack >>= liftIO . putChar . cellAsChar
exec Exit = throwError ExitSuccess
exec _ = error "unexpected opcode"

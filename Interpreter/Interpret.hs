module Interpreter.Interpret (
    interpret
) where

import Bytecode.Ops
import Bytecode.Representable
import Interpreter.Debug
import Interpreter.Error
import Interpreter.LensHelpers
import Interpreter.Machine
import Interpreter.Monad
import Interpreter.Stack
import Interpreter.StackArithmetic
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Data.Vector

interpret :: FunctionSpace -> Machine -> IO ()
interpret funs machine = result >>= \x -> case x of
                                              ExitSuccess -> return ()
                                              err -> putStr "Error: " >> print err
    where steps = forever step
          -- This unconditional match is correct here.  We know that if a Right occured, the next step would be
          -- performed, meaning that if the computation completed, we have a Left.
          result = fmap (\(Left x) -> x) . runExceptT $ steps `runReaderT` funs `evalStateT` machine


step :: MonadStack m => m ()
step = do
    (funname, offset) <- use ip
    fun <- at funname . mapGetter code `viewOr` InvalidFunction
    opcode <- fun !? offset `fromMaybeOr` CodeOutOfBounds
    ip._2 += 1
    exec opcode

doThenPush :: (MonadStack m, NumericRep a, Representable b) => (a -> a -> b) -> a -> a -> m ()
doThenPush f x y = pushValue $ f x y

exec :: forall m. MonadStack m => Op -> m ()
exec (LocalJmp x) = ip._2 .= x
exec (LocalJmpIfZero fmt x) = unaryOp f fmt
    where f :: forall a. NumericRep a => a -> m ()
          f 0 = ip._2 .= x
          f _ = return ()
exec (LoadImmediate x) = pushBS x
exec (Dup fmt) = unaryOp (\x -> pushValue x >> pushValue x) fmt
exec (Swap fmt) = binOp (\x y -> pushValue x >> pushValue y) fmt
exec WriteChar = popValue >>= liftIO . putChar
exec (WriteValue fmt) = unaryOp (liftIO . putStr . show) fmt
exec ReadChar = liftIO getChar >>= pushValue
exec Exit = throwError ExitSuccess
exec (Add fmt) = binOp (doThenPush (+)) fmt
exec (Sub fmt) = binOp (doThenPush (-)) fmt
exec (Mul fmt) = binOp (doThenPush (*)) fmt
exec (Equal fmt) = binOp (doThenPush (==)) fmt
exec (NotEqual fmt) = binOp (doThenPush (/=)) fmt
exec (LessThan fmt) = binOp (doThenPush (<)) fmt
exec (LessEqual fmt) = binOp (doThenPush (<=)) fmt
exec (GreaterThan fmt) = binOp (doThenPush (>)) fmt
exec (GreaterEqual fmt) = binOp (doThenPush (>=)) fmt
exec DumpStack = get >>= dumpStack
exec _ = throwError NotImplementedError

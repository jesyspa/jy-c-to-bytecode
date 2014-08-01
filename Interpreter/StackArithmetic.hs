module Interpreter.StackArithmetic (
    unaryOp,
    binOp,
) where

import Bytecode.Representable
import Interpreter.Stack
import Interpreter.Monad
import Interpreter.FormatDispatch

unaryOp' :: (Representable a, MonadStack m) => (Lambda (Hole -> Constant (m ())) a) -> Proxy a -> m ()
unaryOp' (unwrapf -> f) _ = popValue >>= f

unaryOp :: MonadStack m => (forall a. NumericRep a => a -> m ()) -> Format -> m ()
unaryOp f = dispatch unaryOp' $ wrapf f

binOp' :: (Representable a, MonadStack m) => (Lambda (Hole -> Hole -> Constant (m ())) a) -> Proxy a -> m ()
binOp' (unwrapf -> f) _ = do
    x <- popValue
    y <- popValue
    f x y

binOp :: MonadStack m => (forall a. NumericRep a => a -> a -> m ()) -> Format -> m ()
binOp f = dispatch binOp' $ wrapf f


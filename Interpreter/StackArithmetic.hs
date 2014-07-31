module Interpreter.StackArithmetic (
    binOp,
    monadicOp,
    monadicBinOp
) where

import Bytecode.Representable
import Interpreter.Stack
import Interpreter.Monad
import Interpreter.FormatDispatch

binOp' :: (NumericRep a, MonadStack m) => (Lambda (Hole -> Hole -> Hole) m a) -> Proxy a -> m ()
binOp' (unwrapf -> f) _ = do
    x <- popValue
    y <- popValue
    pushValue $ f x y

binOp :: forall m. MonadStack m => (forall a. NumericRep a => a -> a -> a) -> Format -> m ()
binOp f = dispatch binOp' $ wrapf f

monadicOp' :: (Representable a, MonadStack m) => (Lambda (Hole -> MStack ()) m a) -> Proxy a -> m ()
monadicOp' (unwrapf -> f) _ = popValue >>= f

monadicOp :: MonadStack m => (forall a. NumericRep a => a -> m ()) -> Format -> m ()
monadicOp f = dispatch monadicOp' $ wrapf f

monadicBinOp' :: (Representable a, MonadStack m) => (Lambda (Hole -> Hole -> MStack ()) m a) -> Proxy a -> m ()
monadicBinOp' (unwrapf -> f) _ = do
    x <- popValue
    y <- popValue
    f x y

monadicBinOp :: MonadStack m => (forall a. NumericRep a => a -> a -> m ()) -> Format -> m ()
monadicBinOp f = dispatch monadicBinOp' $ wrapf f

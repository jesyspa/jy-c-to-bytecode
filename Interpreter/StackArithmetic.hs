module Interpreter.StackArithmetic (
    binOp,
    monadicOp,
) where

import Data.Word
import Bytecode.Representable
import Interpreter.Stack
import Interpreter.Monad

binOp' :: (NumericRep a, MonadStack m) => (a -> a -> a) -> Proxy a -> m ()
binOp' f _ = do
        x <- popValue
        y <- popValue
        pushValue $ f x y

binOp :: (MonadStack m) => (forall a. NumericRep a => a -> a -> a) -> Format -> m ()
binOp f = go
    where pr = Proxy
          go Byte = binOp' f (pr :: Proxy Word8)
          go Short = binOp' f (pr :: Proxy Word16)
          go Word = binOp' f (pr :: Proxy Word32)
          go DWord = binOp' f (pr :: Proxy Word64)
          go FWord = binOp' f (pr :: Proxy Float)
          go FDWord = binOp' f (pr :: Proxy Double)

monadicOp' :: (Representable a, MonadStack m) => (a -> m ()) -> Proxy a -> m ()
monadicOp' f _ = popValue >>= f

monadicOp :: (MonadStack m) => (forall a. NumericRep a => a -> m ()) -> Format -> m ()
monadicOp f = go
    where pr = Proxy
          go Byte = monadicOp' f (pr :: Proxy Word8)
          go Short = monadicOp' f (pr :: Proxy Word16)
          go Word = monadicOp' f (pr :: Proxy Word32)
          go DWord = monadicOp' f (pr :: Proxy Word64)
          go FWord = monadicOp' f (pr :: Proxy Float)
          go FDWord = monadicOp' f (pr :: Proxy Double)


module Interpreter.FormatDispatch (
    dispatch,
    wrapf,
    unwrapf,
    Lambda,
    Hole,
    MStack
) where

import Data.Proxy
import Data.Word
import Bytecode.Representable
import Interpreter.Monad

-- We use this as a placeholder for a in the types
data Hole
data MStack a

data family Lambda :: * -> (* -> *) -> * -> *

data instance Lambda (Hole -> MStack ()) m a = Lambda1ToM { getLambda1ToM :: a -> m () }
data instance Lambda (Hole -> Hole -> MStack ()) m a = Lambda2ToM { getLambda2ToM :: a -> a -> m () }
data instance Lambda (Hole -> Hole -> Hole) m a = Lambda2ToA { getLambda2ToA :: a -> a -> a }

class LambdaLike fn where
    type family UnderlyingFun fn (m :: * -> *) a
    wrapf :: UnderlyingFun fn m a -> Lambda fn m a
    unwrapf :: Lambda fn m a -> UnderlyingFun fn m a

instance LambdaLike (Hole -> MStack ()) where
    type UnderlyingFun (Hole -> MStack ()) m a = a -> m ()
    wrapf = Lambda1ToM
    unwrapf = getLambda1ToM

instance LambdaLike (Hole -> Hole -> MStack ()) where
    type UnderlyingFun (Hole -> Hole -> MStack ()) m a = a -> a -> m ()
    wrapf = Lambda2ToM
    unwrapf = getLambda2ToM

instance LambdaLike (Hole -> Hole -> Hole) where
    type UnderlyingFun (Hole -> Hole -> Hole) m a = a -> a -> a
    wrapf = Lambda2ToA
    unwrapf = getLambda2ToA

dispatch :: MonadStack m => (forall a. NumericRep a => Lambda fn m a -> Proxy a -> m ()) -> (forall a. NumericRep a => Lambda fn m a) -> Format -> m ()
dispatch fn f = go
    where pr = Proxy
          go Byte = fn f (pr :: Proxy Word8)
          go Short = fn f (pr :: Proxy Word16)
          go Word = fn f (pr :: Proxy Word32)
          go DWord = fn f (pr :: Proxy Word64)
          go FWord = fn f (pr :: Proxy Float)
          go FDWord = fn f (pr :: Proxy Double)


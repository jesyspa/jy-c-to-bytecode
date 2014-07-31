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

newtype instance Lambda (Hole -> r) m a = LambdaTakingHole (a -> Lambda r m a)
newtype instance Lambda (MStack ()) m a = LambdaMStack { getLambdaMStack :: m () }
newtype instance Lambda Hole m a = LambdaHole { getLambdaHole :: a }

class LambdaLike fn where
    type family UnderlyingFun fn (m :: * -> *) a
    wrapf :: UnderlyingFun fn m a -> Lambda fn m a
    unwrapf :: Lambda fn m a -> UnderlyingFun fn m a

instance LambdaLike r => LambdaLike (Hole -> r) where
    type UnderlyingFun (Hole -> r) m a = a -> UnderlyingFun r m a
    wrapf f = LambdaTakingHole (\x -> wrapf (f x))
    unwrapf (LambdaTakingHole f) = unwrapf . f

instance LambdaLike (MStack ()) where
    type UnderlyingFun (MStack ()) m a = m ()
    wrapf = LambdaMStack
    unwrapf = getLambdaMStack

instance LambdaLike Hole where
    type UnderlyingFun Hole m a = a
    wrapf = LambdaHole
    unwrapf = getLambdaHole

dispatch :: MonadStack m => (forall a. NumericRep a => Lambda fn m a -> Proxy a -> m ()) -> (forall a. NumericRep a => Lambda fn m a) -> Format -> m ()
dispatch fn f = go
    where pr = Proxy
          go Byte = fn f (pr :: Proxy Word8)
          go Short = fn f (pr :: Proxy Word16)
          go Word = fn f (pr :: Proxy Word32)
          go DWord = fn f (pr :: Proxy Word64)
          go FWord = fn f (pr :: Proxy Float)
          go FDWord = fn f (pr :: Proxy Double)


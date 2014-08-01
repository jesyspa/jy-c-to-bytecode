module Interpreter.FormatDispatch (
    dispatch,
    wrapf,
    unwrapf,
    Lambda,
    Hole,
    Constant
) where

import Representation.Representable
import Data.Word

-- We use these two as placeholders in Lambda types.  Hole is evaluated to a, while Constant c is evaluated to c.
-- A lambda should not contain any other types.  (If we need type constructors, those could be added.)
data Hole
data Constant c

data family Lambda :: * -> * -> *

newtype instance Lambda (Hole -> r) a = LambdaTakingHole (a -> Lambda r a)
newtype instance Lambda (Constant c -> r) a = LambdaTakingConstant (c -> Lambda r a)
newtype instance Lambda Hole a = LambdaHole { getLambdaHole :: a }
newtype instance Lambda (Constant c) a = LambdaConstant { getLambdaConstant :: c }

class LambdaLike fn where
    type family UnderlyingFun fn a
    wrapf :: UnderlyingFun fn a -> Lambda fn a
    unwrapf :: Lambda fn a -> UnderlyingFun fn a

instance LambdaLike r => LambdaLike (Hole -> r) where
    type UnderlyingFun (Hole -> r) a = a -> UnderlyingFun r a
    wrapf f = LambdaTakingHole (wrapf . f)
    unwrapf (LambdaTakingHole f) = unwrapf . f

instance LambdaLike r => LambdaLike (Constant c -> r) where
    type UnderlyingFun (Constant c -> r) a = c -> UnderlyingFun r a
    wrapf f = LambdaTakingConstant (wrapf . f)
    unwrapf (LambdaTakingConstant f) = unwrapf . f

instance LambdaLike Hole where
    type UnderlyingFun Hole a = a
    wrapf = LambdaHole
    unwrapf = getLambdaHole

instance LambdaLike (Constant c) where
    type UnderlyingFun (Constant c) a = c
    wrapf = LambdaConstant
    unwrapf = getLambdaConstant

dispatch :: (forall a. NumericRep a => Lambda fn a -> Proxy a -> r) -> (forall a. NumericRep a => Lambda fn a) -> Format -> r
dispatch fn f = go
    where pr = Proxy
          go Byte = fn f (pr :: Proxy Word8)
          go Short = fn f (pr :: Proxy Word16)
          go Word = fn f (pr :: Proxy Word32)
          go DWord = fn f (pr :: Proxy Word64)
          go FWord = fn f (pr :: Proxy Float)
          go FDWord = fn f (pr :: Proxy Double)


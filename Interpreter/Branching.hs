module Interpreter.Branching (
    localJmp,
    localJmpIfZero,
    callFun,
    ret
) where

import Interpreter.Machine
import Interpreter.Monad
import Interpreter.Memory
import Interpreter.Representable
import Interpreter.StackArithmetic
import Control.Lens

localJmp :: MonadStack m => Int -> m ()
localJmp x = ip._2 .= x

localJmpIfZero :: forall m. MonadStack m => Format -> Int -> m ()
localJmpIfZero fmt x = unaryOp f fmt
    where f :: forall a. NumericRep a => a -> m ()
          f 0 = localJmp x
          f _ = return ()

-- Note that we use the same storage for the offset into the function and the function name.  This is to avoid using 8
-- bytes in total; the position overwritten will be in the least significant bytes, which means that this will work
-- unless we have a function 2^56 instructions long.

callFun :: MonadStack m => String -> m ()
callFun fun = do
    (f, i) <- use ip
    af <- use afp
    staticStoreValueAt af i
    staticStoreValueAt af f
    ip .= (fun, 0)

ret :: MonadStack m => m ()
ret = do
    af <- use afp
    i <- staticLoadValueAt af
    f <- staticLoadValueAt af
    ip .= (f, i)

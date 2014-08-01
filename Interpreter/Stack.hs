module Interpreter.Stack (
    popValue,
    pushValue,
    popCells,
    pushCells
) where

import Interpreter.Error
import Interpreter.Machine
import Interpreter.Monad
import Representation.Cell
import Representation.Representable
import Control.Lens
import Control.Monad.Except

popValue' :: (MonadStack m, Representable a) => Proxy a -> m a
popValue' pr = do
    bs <- popCells $ format pr
    fromRepresentation bs `fromMaybeOr` UnrepresentableValueError

popValue :: (MonadStack m, Representable a) => m a
popValue = popValue' (Proxy :: Proxy a)

pushValue :: (MonadStack m, Representable a) => a -> m ()
pushValue = pushCells . toRepresentation

popCells :: MonadStack m => Format -> m [Cell]
popCells fmt = do
    st <- use stack
    let size = formatSize fmt
    when (size > length st) $ throwError StackUnderflow
    let (xs, ys) = splitAt size st
    stack .= ys
    return xs

pushCells :: MonadStack m => [Cell] -> m ()
pushCells xs = stack %= (xs++)


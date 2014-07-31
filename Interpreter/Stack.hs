module Interpreter.Stack (
    popValue,
    pushValue,
    popBS,
    pushBS
) where

import Bytecode.Representable
import Interpreter.Error
import Interpreter.Cell
import Interpreter.Machine
import Interpreter.Monad
import Data.ByteString.Lazy (ByteString, pack, unpack)
import Control.Monad.Except
import Control.Lens

popValue' :: (MonadStack m, Representable a) => Proxy a -> m a
popValue' pr = do
    bs <- popBS $ format pr
    fromRepresentation bs `fromMaybeOr` UnrepresentableValueError

popValue :: (MonadStack m, Representable a) => m a
popValue = popValue' (Proxy :: Proxy a)

pushValue :: (MonadStack m, Representable a) => a -> m ()
pushValue = pushBS . toRepresentation

popBS :: MonadStack m => Format -> m ByteString
popBS fmt = do
    st <- use stack
    let size = formatSize fmt
    when (size > length st) $ throwError StackUnderflow
    let (xs, ys) = splitAt size st
    stack .= ys
    return $ pack $ map cellToWord8 xs

pushBS :: MonadStack m => ByteString -> m ()
pushBS bs = stack %= (xs++)
    where xs = fmap (ByteCell None) $ unpack bs


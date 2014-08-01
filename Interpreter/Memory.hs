module Interpreter.Memory (
    memAlloc,
    staticLoadValueAt,
    loadValueAt,
    staticStoreValueAt,
    storeValueAt
) where

import Interpreter.Error
import Interpreter.FormatDispatch
import Interpreter.Machine
import Interpreter.Monad
import Interpreter.Stack
import Representation.Cell
import Representation.Representable
import Control.Lens
import Control.Monad.Except
import qualified Data.IntMap as IM
import qualified Data.Vector.Mutable as V

chooseAddress :: Int -> IM.IntMap (V.IOVector Cell) -> Int
chooseAddress _ mem | IM.null mem = 0
                    | otherwise = k + V.length v
                        where (k, v) = IM.findMax mem

memAlloc :: MonadStack m => Int -> m ()
memAlloc x = do
    mem <- use heap
    let addr = chooseAddress x mem
    vec <- liftIO $ V.new x
    heap %= IM.insert addr vec
    pushValue addr

staticLoadValueAt :: forall m a. (MonadStack m, Representable a) => Int -> m a
staticLoadValueAt n = do
    mem <- use heap
    (k, vec) <- IM.lookupLE n mem `fromMaybeOr` InvalidRead
    let i = n - k
        sz = formatSize $ format (Proxy :: Proxy a)
    when (i + sz > V.length vec) $ throwError InvalidRead
    repr <- mapM (liftIO . V.read vec) [i..i+sz-1]
    fromRepresentation repr `fromMaybeOr` InvalidRead

loadValueAt' :: forall m a. (MonadStack m, Representable a) => Lambda (Constant Int) a -> Proxy a -> m ()
loadValueAt' (unwrapf -> x) _ = staticLoadValueAt x >>= (pushValue :: a -> m ())

loadValueAt :: MonadStack m => Int -> Format -> m ()
loadValueAt f = dispatch loadValueAt' $ wrapf f

staticStoreValueAt :: forall m a. (MonadStack m, Representable a) => Int -> a -> m ()
staticStoreValueAt n v = do
    mem <- use heap
    (k, vec) <- IM.lookupLE n mem `fromMaybeOr` InvalidWrite
    let i = n - k
        sz = formatSize $ format (Proxy :: Proxy a)
        repr = toRepresentation v
    when (i + sz > V.length vec) $ throwError InvalidWrite
    mapM_ (liftIO . uncurry (V.write vec)) $ zip [i..] repr

storeValueAt' :: forall m a. (MonadStack m, Representable a) => Lambda (Constant Int) a -> Proxy a -> m ()
storeValueAt' (unwrapf -> x) _ = (popValue :: m a) >>= staticStoreValueAt x

storeValueAt :: MonadStack m => Int -> Format -> m ()
storeValueAt f = dispatch storeValueAt' $ wrapf f


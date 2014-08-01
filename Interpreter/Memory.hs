module Interpreter.Memory (
    memAlloc,
    loadValueAt,
    storeValueAt
) where

import Bytecode.Representable
import Interpreter.Cell
import Interpreter.Monad
import Interpreter.Machine
import Interpreter.Stack
import Interpreter.Error
import Interpreter.FormatDispatch
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Vector.Mutable as V
import qualified Data.ByteString.Lazy as BS

chooseAddress :: Int -> M.Map Int (V.IOVector Cell) -> Int
chooseAddress _ mem | M.null mem = 0
                    | otherwise = k + V.length v
                        where (k, v) = M.findMax mem

memAlloc :: MonadStack m => Int -> m ()
memAlloc x = do
    mem <- use heap
    let addr = chooseAddress x mem
    vec <- liftIO $ V.new x
    heap %= M.insert addr vec
    pushValue addr

loadValueAt' :: forall m a. (MonadStack m, Representable a) => Lambda (Constant Int) a -> Proxy a -> m ()
loadValueAt' (unwrapf -> x) pr = do
    mem <- use heap
    (k, vec) <- M.lookupLE x mem `fromMaybeOr` InvalidRead
    let i = x - k
        sz = formatSize $ format pr
    when (i + sz > V.length vec) $ throwError InvalidRead
    repr <- mapM (liftIO . V.read vec) [i..i+sz-1]
    let mval = fromRepresentation $ BS.pack $ map cellToWord8 repr
    val <- mval `fromMaybeOr` InvalidRead :: m a
    pushValue val

loadValueAt :: MonadStack m => Int -> Format -> m ()
loadValueAt f = dispatch loadValueAt' $ wrapf f

storeValueAt' :: forall m a. (MonadStack m, Representable a) => Lambda (Constant Int) a -> Proxy a -> m ()
storeValueAt' (unwrapf -> x) pr = do
    v <- popValue :: m a
    mem <- use heap
    (k, vec) <- M.lookupLE x mem `fromMaybeOr` InvalidWrite
    let i = x - k
        sz = formatSize $ format pr
        repr = map (ByteCell None) $ BS.unpack $ toRepresentation v
    when (i + sz > V.length vec) $ throwError InvalidWrite
    mapM_ (liftIO . uncurry (V.write vec)) $ zip [i..] repr

storeValueAt :: MonadStack m => Int -> Format -> m ()
storeValueAt f = dispatch storeValueAt' $ wrapf f


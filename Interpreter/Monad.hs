module Interpreter.Monad (
    MonadStack,
    fromMaybeOr,
    useOr,
    viewOr
) where

import Bytecode.Ops
import Bytecode.Function
import Interpreter.Error
import Interpreter.Machine
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

type MonadStack m = (MonadReader (FunctionSpace Op) m, MonadState Machine m, MonadError InterpError m, MonadIO m)

fromMaybeOr :: (MonadError e m) => Maybe a -> e -> m a
fromMaybeOr v e = maybe (throwError e) return v

infix 0 `useOr`
useOr :: (MonadError e m, MonadState s m) => Getting (Maybe a) s (Maybe a) -> e -> m a
useOr g e = use g >>= maybe (throwError e) return

infix 0 `viewOr`
viewOr :: (MonadError e m, MonadReader s m) => Getting (Maybe a) s (Maybe a) -> e -> m a
viewOr g e = view g >>= maybe (throwError e) return


module Interpreter.Machine (
    Machine(..),
    Heap,
    afp,
    ip,
    heap,
    stack,
    fromPersistent
) where

import Bytecode.Ops
import Bytecode.Function
import Interpreter.PIResolution
import Representation.Cell
import ObjectCode.Persistent (PersistentMachine(..))
import Control.Lens
import Control.Monad.State
import qualified Data.IntMap as IM
import qualified Data.Vector.Mutable as V

{- Internal Machine Representation:
--
-- Instruction Pointer: code pointer
-- Activation Frame Pointer: data pointer
-- Heap: map of vectors
-- Stack: resizable list of cells
--}

type Heap = IM.IntMap (V.IOVector Cell)

data Machine = Machine
    { _ip :: (String, Int)
    , _afp :: Int
    , _heap :: Heap
    , _stack :: [Cell]
    }

makeLenses ''Machine

fromPersistent :: MonadIO m => PersistentMachine -> m (Machine, FunctionSpace Op)
fromPersistent (PersistentMachine pm pfs) = do
    let ipInit = ("builtin$main", 0)
        afpInit = 0
        stackInit = []
        res = resolvePIAddresses pm
        fs = specifyCode res pfs
    heapInit <- liftIO $ allocate res
    return (Machine ipInit afpInit heapInit stackInit, fs)



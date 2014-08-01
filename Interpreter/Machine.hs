module Interpreter.Machine (
    Machine(..),
    Function(..),
    FunctionSpace,
    afp,
    ip,
    heap,
    stack,
    frameSize,
    code
) where

import Bytecode.Ops
import Interpreter.Cell
import Control.Lens
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V

{- Internal Machine Representation:
 -
 - Instruction Pointer: code pointer
 - Activation Frame Pointer: data pointer
 - Heap: map of vectors
 - Stack: resizable list of cells
 -
 -}
data Machine = Machine
    { _ip :: (String, Int)
    , _afp :: Int
    , _heap :: M.Map Int (V.IOVector Cell)
    , _stack :: [Cell]
    }

makeLenses ''Machine

data Function = Function
    { _frameSize :: Int
    , _code :: V.Vector Op
    } deriving (Eq, Ord, Read, Show)

makeLenses ''Function

type FunctionSpace = M.Map String Function

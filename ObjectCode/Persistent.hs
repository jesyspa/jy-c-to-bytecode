module ObjectCode.Persistent (
    PersistentMachine(..),
    PersistentHeap,
    heap,
    functions
) where

import Representation.Cell
import Bytecode.Ops
import Bytecode.Function
import Control.Lens
import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified Data.Map as M
import qualified Data.Vector as V

type PersistentHeap = M.Map String (V.Vector Cell)

data PersistentMachine = PersistentMachine
    { _heap :: PersistentHeap
    , _functions :: FunctionSpace PIOp
    } deriving (Eq, Ord, Read, Show, Generic)

instance Binary PersistentMachine

makeLenses ''PersistentMachine

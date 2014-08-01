module Bytecode.Function (
    Function(..),
    FunctionSpace,
    frameSize,
    code
) where

import Control.Lens
import Data.Binary (Binary)
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import qualified Data.Map as M
import qualified Data.Vector as V

data Function op = Function
    { _frameSize :: Int
    , _code :: V.Vector op
    } deriving (Eq, Ord, Read, Show, Generic, Functor)

instance Binary op => Binary (Function op)

makeLenses ''Function

type FunctionSpace op = M.Map String (Function op)

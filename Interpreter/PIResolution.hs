module Interpreter.PIResolution (
    resolvePIAddresses,
    allocate,
    specifyCode
) where

import Bytecode.Ops
import Bytecode.Function
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

resolvePIAddresses :: M.Map String (V.Vector a) -> M.Map String (Int, V.Vector a)
resolvePIAddresses = snd . M.mapAccum f 0
    where f n v = (n + V.length v, (n, v))

allocate :: M.Map String (Int, V.Vector a) -> IO (IM.IntMap (MV.IOVector a))
allocate = foldM f IM.empty . M.elems
    where f m (n, v) = (\x -> IM.insert n x m) <$> V.thaw v

specifyCode :: M.Map String (Int, V.Vector a) -> FunctionSpace PIOp -> FunctionSpace Op
specifyCode m = fmap (fmap $ fmap f)
    where f (s, n) = fst (m M.! s) + n

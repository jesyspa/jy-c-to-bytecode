module Interpreter.LensHelpers (
    mapGetter
) where

import Control.Lens

mapGetter :: Functor f => Getting a s a -> Getting (f v) (f s) (f a)
mapGetter = to . fmap . view


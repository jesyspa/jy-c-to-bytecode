module Bytecode.Format (
    Format(..),
    formatSize
) where

import Data.Binary
import GHC.Generics (Generic)

data Format = Byte | Short | Word | DWord | FWord | FDWord
          deriving (Eq, Ord, Read, Show, Generic)

instance Binary Format

formatSize :: Format -> Int
formatSize Byte = 1
formatSize Short = 2
formatSize Word = 4
formatSize DWord = 8
formatSize FWord = 4
formatSize FDWord = 8


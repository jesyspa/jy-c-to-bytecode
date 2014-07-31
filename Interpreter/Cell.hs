module Interpreter.Cell (
    Tag(..),
    Cell(..)
) where

import Data.Word

data Tag = PtrStart | None
         deriving (Eq, Ord, Read, Show)

{- A cell is the smallest addressable unit of memory.  It is either a Word8 or a String.  If it is a Word8, it also has
 - a byte indicating whether it is the first cell of a pointer.
 -}
data Cell = ByteCell Tag Word8 | NameCell String
          deriving (Eq, Ord, Read, Show)


module Interpreter.Cell (
    Tag(..),
    Cell(..),
    cellToWord8,
    word8ToCell,
    cellToStr,
    strToCell
) where

import Data.Word (Word8)

data Tag = PtrStart | None
         deriving (Eq, Ord, Read, Show)

{- A cell is the smallest addressable unit of memory.  It is either a Word8 or a String.  If it is a Word8, it also has
 - a byte indicating whether it is the first cell of a pointer.
 -}
data Cell = ByteCell Tag Word8 | NameCell String
          deriving (Eq, Ord, Read, Show)

cellToWord8 :: Cell -> Word8
cellToWord8 (ByteCell _ x) = x
cellToWord8 (NameCell _) = 0

word8ToCell :: Word8 -> Cell
word8ToCell = ByteCell None

cellToStr :: Cell -> String
cellToStr (ByteCell _ _) = "error$invalidFunPtr"
cellToStr (NameCell x) = x

strToCell :: String -> Cell
strToCell = NameCell

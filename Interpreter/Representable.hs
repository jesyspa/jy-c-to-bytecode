module Interpreter.Representable (
    Representable,
    toRepresentation,
    fromRepresentation,
    format,
    Format(..),
    formatSize,
    Proxy(..),
    NumericRep,
) where

import Bytecode.Format
import Interpreter.Cell
import Data.Binary (Binary, encode, decodeOrFail)
import Data.ByteString.Lazy (ByteString, pack, unpack)
import Data.Proxy
import Data.Word

tryDecode :: Binary a => ByteString -> Maybe a
tryDecode (decodeOrFail -> x) = case x of
    Right (_, _, a) -> Just a
    Left _ -> Nothing

simpleEncode :: Binary a => a -> [Cell]
simpleEncode = map word8ToCell . unpack . encode

simpleDecode :: Binary a => [Cell] -> Maybe a
simpleDecode = tryDecode . pack . map cellToWord8

class Binary a => Representable a where
    toRepresentation :: a -> [Cell]
    toRepresentation = simpleEncode
    fromRepresentation :: [Cell] -> Maybe a
    fromRepresentation = simpleDecode
    format :: Proxy a -> Format

instance Representable Bool where format _ = Byte
instance Representable Char where format _ = Byte
instance Representable Word8 where format _ = Byte
instance Representable Word16 where format _ = Short
instance Representable Int where format _ = DWord
instance Representable Word32 where format _ = Word
instance Representable Word64 where format _ = DWord
instance Representable Float where format _ = FWord
instance Representable Double where format _ = FDWord

instance Representable String where
    toRepresentation = return . strToCell
    fromRepresentation (x:_) = Just $ cellToStr x
    fromRepresentation [] = Nothing
    format _ = Byte

class (Representable a, Eq a, Ord a, Read a, Show a, Num a) => NumericRep a

instance NumericRep Word8
instance NumericRep Word16
instance NumericRep Word32
instance NumericRep Word64
instance NumericRep Float
instance NumericRep Double


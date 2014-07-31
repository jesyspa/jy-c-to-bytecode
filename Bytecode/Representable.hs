module Bytecode.Representable (
    Representable,
    toRepresentation,
    fromRepresentation,
    format,
    Format(..),
    formatSize,
    Proxy(..),
    NumericRep
) where

import Data.Word
import Data.Proxy
import Bytecode.Format
import Data.ByteString.Lazy (ByteString)
import Data.Binary (Binary, encode, decodeOrFail)

class (Binary a, Eq a, Ord a, Read a, Show a) => Representable a where
    toRepresentation :: a -> ByteString
    toRepresentation = encode
    fromRepresentation :: ByteString -> Maybe a
    fromRepresentation x = case decodeOrFail x of
                               Right (_, _, a) -> Just a
                               Left _ -> Nothing
    format :: Proxy a -> Format

instance Representable Char where format _ = Byte
instance Representable Word8 where format _ = Byte
instance Representable Word16 where format _ = Short
instance Representable Int where format _ = DWord
instance Representable Word32 where format _ = Word
instance Representable Word64 where format _ = DWord
instance Representable Float where format _ = FWord
instance Representable Double where format _ = FDWord

class (Representable a, Eq a, Ord a, Read a, Show a, Num a) => NumericRep a

instance NumericRep Word8
instance NumericRep Word16
instance NumericRep Word32
instance NumericRep Word64
instance NumericRep Float
instance NumericRep Double

module Interpreter.Error (
    InterpError(..)
) where

data InterpError =
    MiscError String |
    InvalidFunction | CodeOutOfBounds |
    NotImplementedError |
    StackUnderflow |
    ExitSuccess
    deriving (Eq, Ord, Read, Show)

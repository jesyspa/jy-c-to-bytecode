module Interpreter.Error (
    InterpError(..)
) where

data InterpError =
    MiscError String |
    InvalidFunction | CodeOutOfBounds |
    NotImplementedError |
    StackUnderflow |
    UnrepresentableValueError |
    InvalidRead | InvalidWrite |
    ExitSuccess
    deriving (Eq, Ord, Read, Show)

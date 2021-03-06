module Bytecode.Ops (
    GenOp(..),
    Op,
    PIOp,
    Format(..)
) where

import Bytecode.Format
import Data.Word
import Data.Binary
import GHC.Generics (Generic)

{-     The Abstract Machine
 -
 - The machine consists of five parts: the registers, the stack, the heap, and the code.
 -
 - There are two registers: the instruction pointer and the activation frame pointer.  These point to the next
 - instruction to be executed and to the current activation frame, respectively.
 -
 - The stack is used for evaluating expressions.
 -
 - The heap stores persistent data.  Activation frames are placed here, as well as any objects allocated with malloc.
 - Memory is automatically reclaimed when no pointers into a given allocated block exist.
 -
 - The code is a list of functions, which consist of bytecode instructions, and are annotated with the required
 - activation frame size.
 -
 - Pointers to the heap are represented by integers.
 -
 - Pointers to the code are represented by a pair of function name and offset into the function.
 -
 -     Calling Convention
 -
 - The caller allocates an activation frame of the right size and copies the arguments into it.  It then calls the
 - function.  When the function completes, it leaves the return value in the activation frame and returns control to the
 - caller.  The caller then copies the return value out of the activation frame.
 -
 - An activation frame is layed out as follows:
 -
 -  * Return address
 -  * Previous frame pointer
 -  * Return value
 -  * Arguments
 -  * Locals
 -}

data GenOp label lptr gptr =
    -- Calls
    LocalJmp label | LocalJmpIfZero Format Int | Call String | CallPtr | Return |
    -- Memory management
    MemAlloc Int | MarkPtrOnStack | MarkAbsolutePtr gptr | MarkLocalPtr lptr |
    -- Activation frame pointer operations
    SetAFP | LoadAFP |
    -- Local load/store
    LoadLocal Format lptr| StoreLocal Format lptr |
    -- Absolute load/store
    LoadAbsolute Format gptr | StoreAbsolute Format gptr |
    -- Immediate load
    LoadByte Word8 | LoadShort Word16 | LoadWord Word32 | LoadDWord Word64 | LoadFWord Float | LoadFDWord Double |
    -- Immediate pointer loads
    LoadFun String | LoadAbsolutePtr gptr | LoadLocalPtr lptr |
    -- Stack manipulation
    Dup Format | Swap Format |
    -- Stack arithmetic
    Add Format | Sub Format | Mul Format | Div Format | Mod Format |
    -- Stack bitwise operations
    And Format | Or Format | Xor Format | LeftShift Format | RightShift Format |
    -- Stack comparisons
    Equal Format | NotEqual Format | LessThan Format | LessEqual Format | GreaterThan Format | GreaterEqual Format |
    -- Type manipulations
    Cast Format Format |
    -- Pointer operations
    Deref |
    -- World effects
    ReadChar | WriteChar | ReadValue Format | WriteValue Format | Exit |
    -- Debug
    DumpStack
    deriving (Eq, Ord, Read, Show, Generic, Functor)

type Op = GenOp Int Int Int
type PIOp = GenOp Int Int (String, Int)

instance Binary Op
instance Binary PIOp

module Bytecode.Ops (
    Op(..),
    DataPtr,
    PtrOffset,
    Format(..)
) where

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

type DataPtr = Int
type PtrOffset = Int

data Format = Byte | Short | Word | DWord | FWord | FDWord
          deriving (Eq, Ord, Read, Show)

data Op =
    -- Calls
    LocalJmp Int | LocalJmpIfZero Int | Call String | CallPtr | Return |
    -- Allocation
    MemAlloc Int |
    -- Local load/store
    LoadLocal Format PtrOffset | StoreLocal Format PtrOffset |
    -- Absolute load/store
    LoadAbsolute Format DataPtr | StoreAbsolute Format DataPtr |
    -- Immediate load
    LoadImmediate Format Int | LoadImmediateF Format Double |
    -- Stack arithmetic
    Add Format | Sub Format | Mul Format | Div Format | Mod Format |
    -- Stack bitwise operations
    And Format | Or Format | Xor Format | LeftShift Format | RightShift Format |
    -- Stack comparisons
    Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual |
    -- Type manipulations
    Cast Format Format |
    -- Pointer operations
    Deref |
    -- World effects
    ReadChar | WriteChar | Exit
    deriving (Eq, Ord, Read, Show)

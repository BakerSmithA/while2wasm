
-- Generating WebAssembly from processed While.
-- Demonstrates utility of scoping when compiling to a language which has
-- scoped constructs.

{-# LANGUAGE DeriveFunctor #-}

module Back.CodeGen where

import Transform.Rename.Rename (FreshName)
import Back.WASM
import Helper.Scope.Prog

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- Type of variable from source language.
type SrcVar  = FreshName
-- Type of procedure from source language.
type SrcProc = FreshName

-- Whether variable is stored as a value local to a function, or is passed in
-- as a parameter.
data LocType v
    = Local v
    | Param v
    deriving (Eq, Show)

-- Whether variable the value of a variable is stored in a local variable, or
-- on the stack.
data ValType v
    = Value v
    | Ptr v
    deriving (Eq, Show)

data Emit k
    -- Append an instruction to the end of the current block at innermost scope.
    = Emit WASM k
    -- Get instructions of innermost scoped block. This is the WebAssembly being
    -- constructed.
    | CurrInstr (WASM -> k)
    -- Get name of stack pointer, which can be used to load variables from
    -- function's stack frame.
    | SPName (GlobalName -> k)
    -- Get offset of a local variable from the stack pointer.
    | SPOffset SrcVar (Word -> k)
    -- Returns names of arguments to a function, i.e. the variables in
    -- the **caller** scope that should be pushed onto the stack before calling
    -- the function.
    | CallerScopeFuncArgs SrcProc ([SrcVar] -> k)
    -- Returns the type of a variable, in the current function, given its name.
    -- Used to inform how the variable should be accessed.
    | VarType SrcVar (LocType (ValType SrcVar) -> k)
    deriving Functor

data Block k
    -- Create a new block of instructions that nested emits will append to.
    -- Once scope is exited, this block will be popped from the stack.
    = Block k
    -- Create a new block of instructions that nested emits will append to.
    -- Once scope is exited, the instructions will be placed in a function.
    | Function SrcProc [SrcVar] DoesRet k
    deriving Functor

type CodeGen = Prog Emit Block

emit :: WASM -> CodeGen ()
emit i = Op (Emit i (Var ()))

currInstr :: CodeGen (WASM)
currInstr = Op (CurrInstr Var)

spName :: CodeGen GlobalName
spName = Op (SPName Var)

spOffset :: SrcVar -> CodeGen Word
spOffset v = Op (SPOffset v Var)

callerScopeFuncArgs :: SrcProc -> CodeGen [SrcVar]
callerScopeFuncArgs fname = Op (CallerScopeFuncArgs fname Var)

varType :: SrcVar -> CodeGen (LocType (ValType SrcVar))
varType v = Op (VarType v Var)

-- Returns instructions emitted in the block allowing them to be wrapped up
-- in a WASM control structure, e.g. BLOCK.
codeBlock :: CodeGen () -> CodeGen WASM
codeBlock inner = Scope (fmap (fmap return) (CodeBlock (do inner; currInstr)))

-- Emits nested instructions to a new function.
function :: SrcProc -> [SrcVar] -> DoesRet -> CodeGen a -> CodeGen a
function name args ret body = Scope (fmap (fmap return) (Function name args ret body))

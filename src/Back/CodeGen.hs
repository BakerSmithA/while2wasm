
-- Generating WebAssembly from processed While.
-- Demonstrates utility of scoping when compiling to a language which has
-- scoped constructs.

{-# LANGUAGE DeriveFunctor #-}

module Back.CodeGen where

import Transform.Rename (FreshName)
import Back.WASM
import Helper.Scope.Prog

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- Type of variable from source language.
type SrcVar  = FreshName
-- Type of procedure from source language.
type SrcProc = FreshName

data Emit k
    -- Append an instruction to the end of the current block at innermost scope.
    = Emit (WASM ()) k
    -- Get instructions of innermost scoped block.
    | GetInstr (WASM () -> k)
    -- Get name of stack pointer, which can be used to load variables from
    -- function's stack frame.
    | GetSP (GlobalName -> k)
    -- Get offset of a local variable from the stack pointer.
    | GetSPOffset SrcVar (Word -> k)
    -- Returns names of arguments to a function, i.e. the variables in
    -- the current scope that should be pushed onto the stack before calling
    -- the function.
    | GetArgs SrcProc ([SrcVar] -> k)
    -- Returns the type of a variable, in the current function, given its name.
    -- This informs how the variable should be accessed to push into the stack
    -- before calling another function.
    -- TODO: Wrap var in type.
    | GetVarType SrcVar (SrcVar -> k)
    deriving Functor

data Block k
    -- Create a new block of instructions that nested emits will append to.
    -- Once scope is exited, this block will be popped from the stack.
    = CodeBlock (Instr -> k)
    -- Create a new block of instructions that nested emits will append to.
    -- Once scope is exited, the instructions will be placed in a function.
    | Function FuncName [LocalName] DoesRet k
    deriving Functor

type CodeGen = Prog Emit Block

emit :: WASM () -> CodeGen ()
emit i = Op (Emit i (Var ()))

getInstr :: CodeGen (WASM ())
getInstr = Op (GetInstr Var)

getSP :: CodeGen GlobalName
getSP = Op (GetSP Var)

getSPOffset :: SrcVar -> CodeGen Word
getSPOffset v = Op (GetSPOffset v Var)

getArgs :: SrcProc -> CodeGen [SrcVar]
getArgs fname = Op (GetArgs fname Var)

-- TODO
getVarType :: SrcVar -> CodeGen SrcVar
getVarType v = Op (GetVarType v Var)

-- Returns instructions emitted in the block allowing them to be wrapped up
-- in a WASM control structure, e.g. BLOCK.
codeBlock :: CodeGen () -> CodeGen (WASM ())
codeBlock inner = Scope (fmap (fmap return) (CodeBlock (do inner; getInstr)))

-- Emits nested instructions to a new function.
function :: FuncName -> [LocalName] -> DoesRet -> CodeGen a -> CodeGen a
function name args ret body = Scope (fmap (fmap return) (Function name args ret body))

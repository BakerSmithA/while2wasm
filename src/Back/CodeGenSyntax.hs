
-- Generating WebAssembly from processed While.
-- Demonstrates utility of scoping when compiling to a language which has
-- scoped constructs.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs, DataKinds #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Back.CodeGenSyntax
( SrcVar
, SrcProc
, SrcLocalVars
, SrcParamVars
, LocType(..)
, ValType(..)
, Emit(..)
, Block(..)
, WASM
, wasmName
, emit
, spName
, varSPOffset
, funcVarLocations
, varType
, codeBlock
, function
, emitGetVarAsArg
, emitGetVarVal
, emitSetVarVal
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Back.WASM
import Transform.Rename.Rename (FreshName)
import Helper.Scope.Prog
import Helper.Co

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- Type of variable from source language.
type SrcVar  = FreshName
-- Type of procedure from source language.
type SrcProc = FreshName

-- Variables local to a function.
type SrcLocalVars = Set SrcVar
-- Variables passed into a function.
type SrcParamVars = Set SrcVar
-- Used to lookup where variables are stored in relation to SP.
type SrcSPOffsets = Map SrcVar Word

-- Whether variable is stored as a value local to a function, or is passed in
-- as a parameter.
data LocType v
    = Local v
    | Param v
    deriving (Eq, Show, Functor)

-- Whether variable the value of a variable is stored in a local variable, or
-- on the stack.
data ValType v
    = Val v
    | Ptr v
    deriving (Eq, Show, Functor)

data Emit k
    -- Append an instruction to the end of the current block of current function.
    = Emit WASM k
    -- Get instructions of innermost scoped block. This is the WebAssembly being
    -- constructed.
    | CurrInstr (WASM -> k)
    -- Get name of stack pointer, which can be used to load variables from
    -- function's stack frame.
    | SPName (GlobalName -> k)
    -- Get offset of a local variable from the stack pointer.
    | VarSPOffset SrcVar (Word -> k)
    -- Get local variables and parameters to a function. Used when creating
    -- function definition.
    | FuncVarLocations SrcProc ((SrcLocalVars, SrcParamVars) -> k)
    -- Returns the type of a variable, in the current function, given its name.
    -- Used to inform how the variable should be accessed.
    | VarType SrcVar (LocType (ValType SrcVar) -> k)
    deriving Functor

data Block k
    -- Create a new block of instructions that nested emits will append to.
    -- Once scope is exited, this block will be popped from the stack.
    = CodeBlock k
    -- Create a new block of instructions that nested emits will append to.
    -- Once scope is exited, the instructions will be placed in a function.
    | Function SrcProc DoesRet SrcLocalVars SrcParamVars SrcSPOffsets k
    deriving Functor

emit :: Emit :<: f => WASM -> Prog f g ()
emit i = injectP (Emit i (Var ()))

currInstr :: Emit :<: f => Prog f g WASM
currInstr = injectP (CurrInstr Var)

spName :: Emit :<: f => Prog f g GlobalName
spName = injectP (SPName Var)

varSPOffset :: Emit :<: f => SrcVar -> Prog f g Word
varSPOffset v = injectP (VarSPOffset v Var)

funcVarLocations :: Emit :<: f => SrcProc -> Prog f g (SrcLocalVars, SrcParamVars)
funcVarLocations pname = injectP (FuncVarLocations pname Var)

varType :: Emit :<: f => SrcVar -> Prog f g (LocType (ValType SrcVar))
varType v = injectP (VarType v Var)

-- Returns instructions emitted in the block allowing them to be wrapped up
-- in a WASM control structure, e.g. BLOCK.
codeBlock :: (Functor f, Emit :<: f, Block :<: g) => Prog f g () -> Prog f g WASM
codeBlock inner = injectPSc (fmap (fmap return) (CodeBlock (do inner; currInstr)))

-- Emits nested instructions to a new function.
function :: (Functor f, Block :<: g)
         => SrcProc
         -> DoesRet
         -> SrcLocalVars
         -> SrcParamVars
         -> SrcSPOffsets
         -> Prog f g a
         -> Prog f g a

function pname doesRet locals params spOffsets body
    = injectPSc (fmap (fmap return) (Function pname doesRet locals params spOffsets body ))

--------------------------------------------------------------------------------
-- Convenience functions to make converting from While easier
--------------------------------------------------------------------------------

-- Converts source-code name into name that can be emitted into WASM.
wasmName :: SrcVar -> LocalName
wasmName = show

-- Emit instructions to push an argument to another function onto the stack.
-- I.e. address of a pointer, or value of a value.
emitGetVarAsArg :: (Functor g, Emit :<: f) => LocType (ValType (SrcVar)) -> Prog f g ()
emitGetVarAsArg (Local (Val v)) = emit (getLocal (wasmName v))
emitGetVarAsArg (Param (Val v)) = emit (getLocal (wasmName v))
emitGetVarAsArg (Local (Ptr v)) = emitLocalPtrAddr v
emitGetVarAsArg (Param (Ptr v)) = emit (getLocal (wasmName v))

-- Emit instuction to push the value of a variable onto the stack.
-- I.e. deference a pointer, or value of a value.
emitGetVarVal :: (Functor g, Emit :<: f) => LocType (ValType SrcVar) -> Prog f g ()
emitGetVarVal (Local (Val v)) = emit (getLocal (wasmName v))
emitGetVarVal (Param (Val v)) = emit (getLocal (wasmName v))
emitGetVarVal (Local (Ptr v)) = do emitLocalPtrAddr v; emit (load 0)
emitGetVarVal (Param (Ptr v)) = do emit (getLocal (wasmName v)); emit (load 0)

-- Pushes address of a local varible onto the stack, i.e. some offset from SP.
emitLocalPtrAddr :: (Functor g, Emit :<: f) => SrcVar -> Prog f g ()
emitLocalPtrAddr v = do
    sp     <- spName
    offset <- varSPOffset v
    emit (getGlobal sp)
    emit (constNum (fromIntegral offset))
    emit (binOp SUB)

-- Emit instruction to set value of a variable to value on top of stack.
-- I.e. store value at memory address pointed to, or set value of variable.
emitSetVarVal :: (Functor g, Emit :<: f) => LocType (ValType SrcVar) -> Prog f g () -> Prog f g ()
emitSetVarVal (Local (Val v)) val = do val; emit (setLocal (wasmName v))
emitSetVarVal (Param (Val v)) val = do val; emit (setLocal (wasmName v))
emitSetVarVal (Local (Ptr v)) val = emitSetPtr (emitLocalPtrAddr v) val
emitSetVarVal (Param (Ptr v)) val = emitSetPtr (emit (getLocal (wasmName v))) val

-- Sets the value pointed to by a pointer.
emitSetPtr :: (Functor g, Emit :<: f) => Prog f g () -> Prog f g () -> Prog f g ()
emitSetPtr addr val = do addr; val; emit (store 0)

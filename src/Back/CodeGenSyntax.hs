
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
, GenData(..)
, Function(..)
, spName
, varSPOffset
, funcVarLocations
, varType
, function
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

data GenData k
    -- Get name of stack pointer, which can be used to load variables from
    -- function's stack frame.
    = SPName (GlobalName -> k)
    -- Get offset of a local variable from the stack pointer.
    | VarSPOffset SrcVar (Word -> k)
    -- Get local variables and parameters to a function. Used when creating
    -- function definition.
    | FuncVarLocations SrcProc ((SrcLocalVars, SrcParamVars) -> k)
    -- Returns the type of a variable, in the current function, given its name.
    -- Used to inform how the variable should be accessed.
    | VarType SrcVar (LocType (ValType SrcVar) -> k)
    deriving Functor

data Function k
    -- Continuation will be emitted to a new WebAssembly function.
    = Function SrcProc DoesRet SrcLocalVars SrcParamVars SrcSPOffsets k
    deriving Functor

spName :: GenData :<: f => Prog f g GlobalName
spName = injectP (SPName Var)

varSPOffset :: GenData :<: f => SrcVar -> Prog f g Word
varSPOffset v = injectP (VarSPOffset v Var)

funcVarLocations :: GenData :<: f => SrcProc -> Prog f g (SrcLocalVars, SrcParamVars)
funcVarLocations pname = injectP (FuncVarLocations pname Var)

varType :: GenData :<: f => SrcVar -> Prog f g (LocType (ValType SrcVar))
varType v = injectP (VarType v Var)

-- Emits nested instructions to a new function.
function :: (Functor f, Function :<: g)
         => SrcProc
         -> DoesRet
         -> SrcLocalVars
         -> SrcParamVars
         -> SrcSPOffsets
         -> Prog f g a
         -> Prog f g a

function pname doesRet locals params spOffsets body
    = injectPSc (fmap (fmap return) (Function pname doesRet locals params spOffsets body ))


-- Generating WebAssembly from processed While.
-- Demonstrates utility of scoping when compiling to a language which has
-- scoped constructs.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs, DataKinds #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, KindSignatures #-}

module Back.CodeGen
( SrcVar
, SrcProc
, LocType(..)
, ValType(..)
, Emit
, Block
, spName
, varSPOffset
, funcVars
, varType
, funcScope
, handleCodeGen
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
type SrcVar = FreshName
-- Type of procedure from source language.
type SrcProc = FreshName

-- Offset from stack pointer. Used to location variables on the stack.
type SPOffset = Word

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
    -- Emit a WebAssembly function containing the given WASM as its body.
    = EmitFunc FuncName DoesRet [LocalName] [LocalName] WASM k
    -- Get offset of a local variable from the stack pointer.
    | VarSPOffset SrcVar (Word -> k)
    -- Returns the type of a variable, in the current function, given its name.
    -- Used to inform how the variable should be accessed.
    | VarType SrcVar (LocType (ValType SrcVar) -> k)
    -- Get name of stack pointer, which can be used to load variables from
    -- function's stack frame.
    | SPName (GlobalName -> k)
    -- Get local variables and parameters to a function. Used when emitting a
    -- or calling a function.
    | FuncVars SrcProc (([SrcVar], [SrcVar]) -> k)
    deriving Functor

data Block k
    -- Inside scope, use of VarSPOffset and VarType will use functions
    -- passed to FuncScope.
    = FuncScope (SrcVar -> LocType (ValType SrcVar)) (SrcVar -> SPOffset) k
    deriving Functor

emitFunc :: Emit :<: f => FuncName -> DoesRet -> [LocalName] -> [LocalName] -> WASM -> Prog f g ()
emitFunc name ret locals params body = injectP (EmitFunc name ret locals params body (Var ()))

spName :: Emit :<: f => Prog f g GlobalName
spName = injectP (SPName Var)

varSPOffset :: Emit :<: f => SrcVar -> Prog f g Word
varSPOffset v = injectP (VarSPOffset v Var)

funcVars :: Emit :<: f => SrcProc -> Prog f g ([SrcVar], [SrcVar])
funcVars pname = injectP (FuncVars pname Var)

varType :: Emit :<: f => SrcVar -> Prog f g (LocType (ValType SrcVar))
varType v = injectP (VarType v Var)

funcScope :: (Functor f, Block :<: g)
          => (SrcVar -> LocType (ValType SrcVar))
          -> (SrcVar -> SPOffset)
          -> Prog f g a
          -> Prog f g a
funcScope varType spOffset inner
    = injectPSc (fmap (fmap return) (FuncScope varType spOffset inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Env = Env

data Carrier a n = CG { runCG :: Env -> (Carrier' a n, Env) }

data Carrier' a :: Nat -> * where
    CZ :: a -> Carrier' a 'Z
    CS :: (Env -> (Carrier' a n, Env)) -> Carrier' a ('S n)

gen :: a -> Carrier a 'Z
gen wasm = CG (\env -> (CZ wasm, env))

alg :: Alg Emit Block (Carrier a)
alg = A a d p where
    a :: Emit (Carrier a n) -> Carrier a n
    a (VarType v fk) = CG $ \env -> runCG (fk (Local (Val v))) env

    d :: Block (Carrier a ('S n)) -> Carrier a n
    d = undefined

    p :: Carrier a n -> Carrier a ('S n)
    p = undefined

handleCodeGen :: Prog Emit Block a -> (a, [Func])
handleCodeGen prog = case runCG (run gen alg prog) Env of
    (CZ wasm, env) -> (wasm, [])

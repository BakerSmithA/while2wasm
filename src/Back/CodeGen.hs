
-- Generating WebAssembly from processed While.
-- Demonstrates utility of scoping when compiling to a language which has
-- scoped constructs.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs, DataKinds #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, KindSignatures #-}

module Back.CodeGen
( SrcVar
, SrcProc
, SPOffset
, LocType(..)
, ValType(..)
, Emit
, Block
, Env(..)
, emitFunc
, spName
, varSPOffset
, funcVars
, varType
, dirtyVars
, funcScope
, handleCodeGen
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Back.WASM hiding (funcs)
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
    = EmitFunc Func k
    -- Get offset of a local variable from the stack pointer.
    | VarSPOffset SrcVar (Word -> k)
    -- Returns the type of a variable, in the current function, given its name.
    -- Used to inform how the variable should be accessed.
    | VarType SrcVar (LocType (ValType SrcVar) -> k)
    -- Get name of stack pointer, which can be used to load variables from
    -- function's stack frame.
    | SPName (GlobalName -> k)
    -- Return variables which are modified in two different scopes.
    -- In WebAssembly, these variables are stored on the stack.
    | DirtyVars (Set SrcVar -> k)
    -- Get local variables and parameters to a function. Used when emitting a
    -- or calling a function.
    | FuncVars SrcProc ((Set SrcVar, Set SrcVar) -> k)
    deriving Functor

data Block k
    -- Inside scope, use of VarSPOffset and VarType will use functions
    -- passed to FuncScope.
    = FuncScope (SrcVar -> LocType (ValType SrcVar)) (SrcVar -> SPOffset) k
    deriving Functor

emitFunc :: Emit :<: f => Func -> Prog f g ()
emitFunc func = injectP (EmitFunc func (Var ()))

spName :: Emit :<: f => Prog f g GlobalName
spName = injectP (SPName Var)

varSPOffset :: Emit :<: f => SrcVar -> Prog f g Word
varSPOffset v = injectP (VarSPOffset v Var)

funcVars :: Emit :<: f => SrcProc -> Prog f g (Set SrcVar, Set SrcVar)
funcVars pname = injectP (FuncVars pname Var)

varType :: Emit :<: f => SrcVar -> Prog f g (LocType (ValType SrcVar))
varType v = injectP (VarType v Var)

dirtyVars :: Emit :<: f => Prog f g (Set SrcVar)
dirtyVars = injectP (DirtyVars Var)

funcScope :: (Functor f, Block :<: g)
          => (SrcVar -> LocType (ValType SrcVar))
          -> (SrcVar -> SPOffset)
          -> Prog f g a
          -> Prog f g a
funcScope varType spOffset inner
    = injectPSc (fmap (fmap return) (FuncScope varType spOffset inner))

--------------------------------------------------------------------------------
-- Aux Semantics
--------------------------------------------------------------------------------

data Env = Env {
    funcs        :: [Func]
  , currVarType  :: SrcVar -> (LocType (ValType SrcVar))
  , spOffset     :: SrcVar -> SPOffset
  , globalSPName :: GlobalName
  , dirty        :: Set SrcVar
  , funcVarLocs  :: Map SrcProc (Set SrcVar, Set SrcVar)
}

addFunc :: Func -> Env -> Env
addFunc func env = env { funcs = func:(funcs env) }

restoreEnv :: Env -> Env -> Env
restoreEnv old new = old { funcs = funcs new }

lookupFuncVars :: SrcProc -> Env -> (Set SrcVar, Set SrcVar)
lookupFuncVars pname env =
    case Map.lookup pname (funcVarLocs env) of
        Nothing   -> error ("No function named " ++ show pname)
        Just locs -> locs

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier a n = CG { runCG :: Env -> (Carrier' a n, Env) }

data Carrier' a :: Nat -> * where
    CZ :: a -> Carrier' a 'Z
    CS :: (Env -> (Carrier' a n, Env)) -> Carrier' a ('S n)

gen :: a -> Carrier a 'Z
gen wasm = CG (\env -> (CZ wasm, env))

-- NOTE: Pattern matching on Other is not required here because this is not a
-- composite effect handler.
alg :: Alg Emit Block (Carrier a)
alg = A a d p where
    a :: Emit (Carrier a n) -> Carrier a n
    a (EmitFunc func k)   = CG $ \env -> runCG k (addFunc func env)
    a (VarSPOffset v fk)  = CG $ \env -> runCG (fk (spOffset env v)) env
    a (VarType v fk)      = CG $ \env -> runCG (fk (currVarType env v)) env
    a (SPName fk)         = CG $ \env -> runCG (fk (globalSPName env)) env
    a (DirtyVars fk)      = CG $ \env -> runCG (fk (dirty env)) env
    a (FuncVars pname fk) = CG $ \env -> runCG (fk (lookupFuncVars pname env)) env

    d :: Block (Carrier a ('S n)) -> Carrier a n
    d (FuncScope varType spOffset inner) = CG $ \env ->
        let env' = env { currVarType=varType, spOffset=spOffset }
        in case runCG inner env of
            -- Make new environment contain emitted functions.
            (CS k, env'') -> k (restoreEnv env env'')

    p :: Carrier a n -> Carrier a ('S n)
    p (CG runCG) = CG $ \env -> (CS runCG, env)

handleCodeGen :: Env -> Prog Emit Block a -> (a, [Func])
handleCodeGen env prog =
    case runCG (run gen alg prog) env of
        (CZ wasm, env') -> (wasm, funcs env')

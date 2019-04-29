
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
, FuncScope
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
import Helper.Scope.Nest
import Helper.Co
import Helper.Inj
import Helper.Eff
import Helper.Eff.Reader
import Helper.Eff.Writer

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
    -- CodeGen a WebAssembly function containing the given WASM as its body.
    = EmitFunc' Func k
    -- Get offset of a local variable from the stack pointer.
    | VarSPOffset' SrcVar (Word -> k)
    -- Returns the type of a variable, in the current function, given its name.
    -- Used to inform how the variable should be accessed.
    | VarType' SrcVar (LocType (ValType SrcVar) -> k)
    -- Get name of stack pointer, which can be used to load variables from
    -- function's stack frame.
    | SPName' (GlobalName -> k)
    -- Return variables which are modified in two different scopes.
    -- In WebAssembly, these variables are stored on the stack.
    | DirtyVars' (Set SrcVar -> k)
    -- Get local variables and parameters to a function. Used when emitting a
    -- or calling a function.
    | FuncVars' SrcProc ((Set SrcVar, Set SrcVar) -> k)
    deriving Functor

data FuncScope k
    -- Inside scope, use of VarSPOffset and VarType will use functions
    -- passed to FuncScope.
    = FuncScope' (SrcVar -> LocType (ValType SrcVar)) (SrcVar -> SPOffset) k
    deriving Functor

pattern EmitFunc f k <- (prj -> Just (EmitFunc' f k))
emitFunc :: Emit :<: f => Func -> Prog f g ()
emitFunc func = injectP (EmitFunc' func (Var ()))

pattern SPName k <- (prj -> Just (SPName' k))
spName :: Emit :<: f => Prog f g GlobalName
spName = injectP (SPName' Var)

pattern VarSPOffset v k <- (prj -> Just (VarSPOffset' v k))
varSPOffset :: Emit :<: f => SrcVar -> Prog f g Word
varSPOffset v = injectP (VarSPOffset' v Var)

pattern FuncVars p k <- (prj -> Just (FuncVars' p k))
funcVars :: Emit :<: f => SrcProc -> Prog f g (Set SrcVar, Set SrcVar)
funcVars pname = injectP (FuncVars' pname Var)

pattern VarType v k <- (prj -> Just (VarType' v k))
varType :: Emit :<: f => SrcVar -> Prog f g (LocType (ValType SrcVar))
varType v = injectP (VarType' v Var)

pattern DirtyVars k <- (prj -> Just (DirtyVars' k))
dirtyVars :: Emit :<: f => Prog f g (Set SrcVar)
dirtyVars = injectP (DirtyVars' Var)

pattern FuncScope varType spOffset k <- (prj -> Just (FuncScope' varType spOffset k))
funcScope :: (Functor f, FuncScope :<: g)
          => (SrcVar -> LocType (ValType SrcVar))
          -> (SrcVar -> SPOffset)
          -> Prog f g a
          -> Prog f g a
funcScope varType spOffset inner
    = injectPSc (fmap (fmap return) (FuncScope' varType spOffset inner))

--------------------------------------------------------------------------------
-- Aux Semantics
--------------------------------------------------------------------------------

data Env = Env {
      currVarType  :: SrcVar -> (LocType (ValType SrcVar))
    , spOffset     :: SrcVar -> SPOffset
    , globalSPName :: GlobalName
    , dirty        :: Set SrcVar
    , funcVarLocs  :: Map SrcProc (Set SrcVar, Set SrcVar)
}

lookupFuncVars :: SrcProc -> Env -> (Set SrcVar, Set SrcVar)
lookupFuncVars pname env =
    case Map.lookup pname (funcVarLocs env) of
        Nothing   -> error ("No function named " ++ show pname)
        Just locs -> locs

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Ctx     f g = Prog (Tell [Func] :+: Ask Env :+: f) (LocalR Env :+: g)
type Carrier f g = Nest1 (Ctx f g)

gen :: (Functor f, Functor g) => a -> Carrier f g a 'Z
gen x = Nest1 (return (NZ1 x))

alg :: (Functor f, Functor g) => Alg (Emit :+: f) (FuncScope :+: g) (Carrier f g a)
alg = A a d p where
    a :: (Functor f, Functor g) => (Emit :+: f) (Carrier f g a n) -> Carrier f g a n
    a (EmitFunc func k)   = Nest1 $ tell [func] >> runNest1 k
    a (VarSPOffset v fk)  = Nest1 $ ask >>= \env -> runNest1 (fk (spOffset env v))
    a (VarType v fk)      = Nest1 $ ask >>= \env -> runNest1 (fk (currVarType env v))
    a (SPName fk)         = Nest1 $ ask >>= \env -> runNest1 (fk (globalSPName env))
    a (DirtyVars fk)      = Nest1 $ ask >>= \env -> runNest1 (fk (dirty env))
    a (FuncVars pname fk) = Nest1 $ ask >>= \env -> runNest1 (fk (lookupFuncVars pname env))
    a (Other op)          = Nest1 (Op (fmap runNest1 (R (R op))))

    d :: (Functor f, Functor g) => (FuncScope :+: g) (Carrier f g a ('S n)) -> Carrier f g a n
    d (FuncScope varType spOffset inner) = Nest1 $ do
        env <- ask
        let env' = env { currVarType=varType, spOffset=spOffset }
        -- By using localR, currVarType and spOffset are automatically restored.
        -- Since a writer is used to keep track of functions, it is automatically
        -- persisted.
        NS1 runK' <- localR env' (runNest1 inner)
        runK'

    d (Other op) = Nest1 (Scope (fmap (\(Nest1 prog) -> fmap f prog) (R op))) where
        f :: (Functor f, Functor g) => Nest1' (Ctx f g) a ('S n) -> Ctx f g (Nest1' (Ctx f g) a n)
        f (NS1 prog) = prog

    p :: (Functor f, Functor g) => Carrier f g a n -> Carrier f g a ('S n)
    p (Nest1 runNest1) = Nest1 (return (NS1 runNest1))

delegate :: (Functor f, Functor g) => Prog (Emit :+: f) (FuncScope :+: g) a -> Ctx f g a
delegate prog = case run gen alg prog of
    (Nest1 prog') -> fmap (\(NZ1 x) -> x) prog'

handleCodeGen :: (Functor f, Functor g) => Env -> Prog (Emit :+: f) (FuncScope :+: g) a -> Prog f g (a, [Func])
handleCodeGen env = handleReader env . handleWriter . delegate

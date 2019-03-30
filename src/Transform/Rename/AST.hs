
-- Supplies AST nodes needed to describe AST after renaming.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}

module Transform.Rename.AST where

import Data.Word (Word)
import Front.AST
import Helper.Co

type Fresh = Word

data FVarExp k
    = FGetVar Fresh
    deriving Functor

data FVarStm k
    = FSetVar Fresh k k
    deriving Functor

data FProcStm k
    = FCall Fresh k
    deriving Functor

data FBlockStm k
    = FBlock (VarDecls Fresh k) (ProcDecls Fresh k) k
    deriving Functor

-- Smart constructors.

getFVar :: FVarExp :<: f => Fresh -> Prog f g a
getFVar v = inject (FGetVar v)

setFVar :: FVarStm :<: f => Fresh -> Prog f g () -> Prog f g ()
setFVar v x = inject (FSetVar v x (Var ()))

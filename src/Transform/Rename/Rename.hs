
-- Renames variables in AST to be represented using unique numbers instead of
-- strings.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts  #-}

module Transform.Rename
( FreshName(..)
, rename
) where

import Front.AST
import Transform.Rename.RenameEff
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff
import Helper.Eff.Void

import Front.Pretty
import Helper.Pretty

-- To allow two versions of Fresh and Rename to exist in context, i.e. for
-- variables and procedures, need to be able to distinguish between them.
newtype VarName  = VarName  Ident deriving (Eq, Ord, Show)
newtype ProcName = ProcName Ident deriving (Eq, Ord, Show)

-- Context in which renaming is performed. This provides fresh names and scope
-- for renaming.
type Op  = Fresh  VarName :+: Fresh  ProcName :+: Void
type Sc  = Rename VarName :+: Rename ProcName :+: Void
type Ctx = Prog Op Sc

instance VarExp FreshName :<: f => FreeAlg (VarExp Ident) (Ctx (Free f a)) where
    alg (GetVar v) = do
        v' <- fresh (VarName v)
        return (getVar v')

instance AExp :<: f => FreeAlg AExp (Ctx (Free f a)) where
    alg (Num n)   = return (num n)
    alg (Add x y) = add <$> x <*> y
    alg (Sub x y) = sub <$> x <*> y
    alg (Mul x y) = mul <$> x <*> y

instance BExp :<: f => FreeAlg BExp (Ctx (Free f a)) where
    alg (T)       = return true
    alg (F)       = return false
    alg (Equ x y) = equ  <$> x <*> y
    alg (LEq x y) = leq  <$> x <*> y
    alg (And x y) = andB <$> x <*> y
    alg (Not x)   = notB <$> x

instance VarStm FreshName :<: f => FreeAlg (VarStm Ident) (Ctx (Free f a)) where
    alg (SetVar v x) = do
        v' <- fresh (VarName v)
        rnX <- x
        return (setVar v' rnX)

instance ProcStm FreshName :<: f => FreeAlg (ProcStm Ident) (Ctx (Free f a)) where
    alg (Call pname) = do
        pname' <- fresh (ProcName pname)
        return (call pname')

instance Stm :<: f => FreeAlg Stm (Ctx (Free f a)) where
    alg (Skip)            = return skip
    alg (Export x)        = export <$> x
    alg (If cond t e)     = ifElse <$> cond <*> t <*> e
    alg (While cond body) = while  <$> cond <*> body
    alg (Comp s1 s2)      = comp   <$> s1 <*> s2

instance BlockStm FreshName FreshName :<: f => FreeAlg (BlockStm Ident Ident) (Ctx (Free f a)) where
    alg (Block varDecls procDecls body) = rename (map VarName (fsts varDecls)) $ do
        rnVarDecls  <- map2M (fresh . VarName) id varDecls
        rnProcDecls <- map2M (fresh . VarName) id procDecls
        rnBody      <- body
        return (block rnVarDecls rnProcDecls rnBody)

makeRename :: (Functor g, FreeAlg f (Ctx (Free g a))) => Free f a -> Ctx (Free g a)
makeRename = evalF (return . return)

renameAST :: (Functor g, FreeAlg f (Ctx (Free g a))) => Free f a -> Free g a
-- Handle renaming variable and procedures.
renameAST = handleVoid . handleRename . handleRename . makeRename

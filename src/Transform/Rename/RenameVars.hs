
-- Renames variables in AST to be represented using unique numbers instead of
-- strings.

-- NOTE: Demonstrates renaming of just variables.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts  #-}
{-# LANGUAGE DeriveFunctor #-}

module Transform.Rename.Rename
( FreshName
, renameAST
) where

import Front.AST hiding (BlockStm(..), block)
import Transform.Rename.RenameEff
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void
import Helper.Eff.Exception

-- Context in which renaming is performed. This provides fresh names and scope
-- for renaming. Also provides execptions, which are thrown if a procedure has
-- not been seen before.
type Op      = Fresh  Ident :+: Void
type Sc      = Rename Ident :+: Void
type Carrier = Prog Op Sc

instance VarExp FreshName :<: f => FreeAlg (VarExp Ident) (Carrier (Free f a)) where
    alg (GetVar v) = do
        v' <- fresh v
        return (getVar v')

instance AExp :<: f => FreeAlg AExp (Carrier (Free f a)) where
    alg (Num n)   = return (num n)
    alg (Add x y) = add <$> x <*> y
    alg (Sub x y) = sub <$> x <*> y
    alg (Mul x y) = mul <$> x <*> y

instance BExp :<: f => FreeAlg BExp (Carrier (Free f a)) where
    alg (T)       = return true
    alg (F)       = return false
    alg (Equ x y) = equ  <$> x <*> y
    alg (LEq x y) = leq  <$> x <*> y
    alg (And x y) = andB <$> x <*> y
    alg (Not x)   = notB <$> x

instance VarStm FreshName :<: f => FreeAlg (VarStm Ident) (Carrier (Free f a)) where
    alg (SetVar v x) = do
        v' <- fresh v
        rnX <- x
        return (setVar v' rnX)

instance Stm :<: f => FreeAlg Stm (Carrier (Free f a)) where
    alg (Skip)            = return skip
    alg (Export x)        = export <$> x
    alg (If cond t e)     = ifElse <$> cond <*> t <*> e
    alg (While cond body) = while  <$> cond <*> body
    alg (Comp s1 s2)      = comp   <$> s1   <*> s2

-- NOTE: Only here for demonstration purposes, so procedures are not included.
-- Block with local variable and procedure declarations, where the names of
-- variables and procedures are represented as strings.
data BlockStm v k
    = Block (VarDecls v k) k
    deriving (Functor, Eq, Show)

block ::  BlockStm v :<: f => [(v, Free f a)] -> Free f a -> Free f a
block varDecls body = injectF (Block varDecls body)

instance BlockStm FreshName :<: f => FreeAlg (BlockStm Ident) (Carrier (Free f a)) where
    alg (Block varDecls body) =
        rename (fsts varDecls) $ do
            rnVarDecls  <- map2M (fresh) id varDecls
            rnBody      <- body
            return (block rnVarDecls rnBody)

makeRename :: (Functor g, FreeAlg f (Carrier (Free g a))) => Free f a -> Carrier (Free g a)
makeRename = evalF (return . return)

renameAST :: (Functor g, FreeAlg f (Carrier (Free g a))) => Free f a -> Free g a
-- Handle renaming variable and procedures.
renameAST = handleVoid . handleRename . makeRename

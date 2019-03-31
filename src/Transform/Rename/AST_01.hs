
-- Investigation of methods used to map between representations of the AST.
-- In this method, the Datatypes a la Carte methods are used to create instances
-- of typeclasses which describe how to fold over the AST to create the new AST.
--
-- An advantage of this method is the ease of specifying what the new and old
-- AST contains, i.e. using (:<:). The compiler also picks up errors if try to
--
-- return a non-fresh variable (but this is common to all methods in investigation).
-- A disadvantage is the mapping needs to be specified for every datatype
-- that will be mapped over into the tree. Even if no renaming occurs, such as
-- is the case for AExp.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Transform.Rename.AST where

import Front.AST
import Transform.Rename.RenameEff
import Helper.Alg
import Helper.Co
import Helper.Eff.Void

type Handler f g = CarrierId (Prog (Rename :+: Void) (Local :+: Void) (Prog f g ()))

-- Convenience method for making it easier to convert binary operators.
binOp :: (Prog f g () -> Prog f g () -> Prog f g ()) -> Handler f g n -> Handler f g n -> Handler f g n
binOp f (Id x) (Id y) = Id $ do
    x' <- x; y' <- y
    return (f x' y')

instance VarExp Fresh :<: f => OpAlg (VarExp Ident) (Handler f g) where
    alg (GetVar v) = Id $ do
        v' <- varName v
        return (getVar v')

instance AExp :<: f => OpAlg AExp (Handler f g) where
    alg (Num n)   = Id $ return (num n)
    alg (Add x y) = binOp add x y
    alg (Sub x y) = binOp sub x y
    alg (Mul x y) = binOp mul x y

instance BExp :<: f => OpAlg BExp (Handler f g) where
    alg (T)          = Id $ return true
    alg (F)          = Id $ return false
    alg (Equ x y)    = binOp equ x y
    alg (LEq x y)    = binOp leq x y
    alg (And x y)    = binOp andB x y
    alg (Not (Id x)) = Id $ do x' <- x; return (notB x')

instance (VarStm Fresh :<: f, Functor g) => OpAlg (VarStm Ident) (Handler f g) where
    alg (SetVar v (Id x) (Id k)) = Id $ do
        v' <- varName v
        x' <- x
        k' <- k
        return (do setVar v' x'; k')

instance (ProcStm Fresh :<: f, Functor g) => OpAlg (ProcStm Ident) (Handler f g) where
    alg (Call p (Id k)) = Id $ do
        p' <- procName p
        k' <- k
        return (do call p'; k')

instance (Stm :<: f, Functor g) => OpAlg Stm (Handler f g) where
    alg (Skip (Id k)) = Id $ do
        k' <- k
        return (do skip; k')

    alg (Export (Id x) (Id k)) = Id $ do
        x' <- x
        k' <- k
        return (do export x'; k')

instance (Functor f, ScopeStm :<: g) => ScopeAlg ScopeStm (Handler f g) where
    dem (If (Id b) (Id t) (Id e)) = Id $ do
        b' <- b
        t' <- t
        e' <- e
        return (ifElse b' t' e')

    dem (While (Id b) (Id s)) = Id $ do
        b' <- b
        s' <- s
        return (while b' s')

instance (Functor f, BlockStm Fresh Fresh :<: g) => ScopeAlg (BlockStm Ident Ident) (Handler f g) where
    dem (Block vs ps (Id s)) = Id $ do
        vs' <- map2M varName unId vs
        ps' <- map2M procName unId ps
        s'  <- s
        return (block vs' ps' s')

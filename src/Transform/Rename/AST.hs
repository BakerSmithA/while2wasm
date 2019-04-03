
-- Investigation of methods used to map between representations of the AST.
-- In this method, the Datatypes a la Carte methods are used to create instances
-- of typeclasses which describe how to fold over the AST to create the new AST.
--
-- An advantage of this method is it is easy to add new mappings from syntax
-- by creating a new typeclass instance.
--
-- A disadvantage is the mapping needs to be specified for every datatype
-- that will be mapped over into the tree. Even if no renaming occurs, such as
-- is the case for AExp.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Transform.Rename.AST
( RenameHandler
, Carrier
, makeRn
) where

import Control.Monad (liftM2)
import Front.AST
import Transform.Rename.RenameEff
import Helper.Alg
import Helper.Co
import Helper.Eff.Void
import Helper.Eff.Exception

-- To allow two different instance of Rename in the handler, use newtypes.
-- This allows procedures to be renamed separately from variables.
newtype VarName  = VarName  Ident deriving (Eq, Ord)
newtype ProcName = ProcName Ident deriving (Eq, Ord)

type Op                = Throw :+: Rename VarName    :+: Rename ProcName :+: Void
type Sc                = Catch :+: LocalName VarName :+: LocalName ProcName :+: Void
type RenameHandler f g = Prog Op Sc (Prog f g ())
type Carrier       f g = CarrierId (RenameHandler f g)

binOp :: (Prog f g () -> Prog f g () -> Prog f g ()) -> Carrier f g n -> Carrier f g n -> Carrier f g n
binOp f (Id x) (Id y) = Id (liftM2 f x y)

instance VarExp FreshName :<: f => OpAlg (VarExp Ident) (Carrier f g) where
    alg (GetVar v) = Id $ do
        v' <- name (VarName v)
        return (getVar v')

instance AExp :<: f => OpAlg AExp (Carrier f g) where
    alg (Num n)   = Id $ return (num n)
    alg (Add x y) = binOp add x y
    alg (Sub x y) = binOp sub x y
    alg (Mul x y) = binOp mul x y

instance BExp :<: f => OpAlg BExp (Carrier f g) where
    alg (T)          = Id $ return true
    alg (F)          = Id $ return false
    alg (Equ x y)    = binOp equ x y
    alg (LEq x y)    = binOp leq x y
    alg (And x y)    = binOp andB x y
    alg (Not (Id x)) = Id $ do x' <- x; return (notB x')

instance (VarStm FreshName :<: f, Functor g) => OpAlg (VarStm Ident) (Carrier f g) where
    alg (SetVar v (Id x) (Id k)) = Id $ do
        v' <- name (VarName v)
        x' <- x
        k' <- k
        return (do setVar v' x'; k')

instance (ProcStm FreshName :<: f, Functor g) => OpAlg (ProcStm Ident) (Carrier f g) where
    alg (Call p (Id k)) = Id $ do
        -- If a procedure has not been declared before calling, then throw an error.
        exists <- exists (ProcName p)
        if exists
            then do
                p' <- name (ProcName p)
                k' <- k
                return (do call p'; k')
            else
                throw "No procedure declared"

instance (Stm :<: f, Functor g) => OpAlg Stm (Carrier f g) where
    alg (Skip (Id k)) = Id $ do
        k' <- k
        return (do skip; k')

    alg (Export (Id x) (Id k)) = Id $ do
        x' <- x
        k' <- k
        return (do export x'; k')

instance (Functor f, ScopeStm :<: g) => ScopeAlg ScopeStm (Carrier f g) where
    dem (If (Id b) (Id t) (Id e)) = Id $ do
        b' <- b
        t' <- t
        e' <- e
        return (ifElse b' t' e')

    dem (While (Id b) (Id s)) = Id $ do
        b' <- b
        s' <- s
        return (while b' s')

instance (Functor f, BlockStm FreshName FreshName :<: g) => ScopeAlg (BlockStm Ident Ident) (Carrier f g) where
    dem (Block vs ps (Id s)) = Id $ do
        localNames (map VarName (fsts vs)) (do
            localNames (map ProcName (fsts ps)) (do
                vs' <- map2M (name . VarName) unId vs
                ps' <- map2M (name . ProcName) unId ps
                s'  <- s
                return (block vs' ps' s')))

-- Use Datatypes a la Carte to convert AST to handler, the types in the AST
-- are not the same before and after, indicated by the change from Prog f g to Prog h i.
makeRn :: (Functor h, Functor i)
     => (OpAlg f (Carrier h i), ScopeAlg g (Carrier h i))
     => Prog f g () -> RenameHandler h i
makeRn = evalId gen where
    gen x = Id (return (return x))

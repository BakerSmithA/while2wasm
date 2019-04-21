
-- Used to define algebras over Prog for use with Datatypes a la Carte methods.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE DataKinds, KindSignatures, RankNTypes #-}

module Helper.Scope.Alg
( module Helper.Scope.Prog
, OpAlg(..)
, ScopeAlg(..)
, Pro
, eval
, evalId
, evalHandler
) where

import Helper.Scope.Prog
import Helper.Co

-- Algebra for Op constructor of Prog.
class Functor f => OpAlg f (a :: Nat -> *) where
    alg :: f (a n) -> a n

class Functor g => ScopeAlg g a where
    dem :: g (a ('S n)) -> a n

-- `pro` is not included in the ScopeAlg type class, and instead is given to
-- `eval` below. This is because `g` is not used in the type of `pro` and
-- therefore the type of `g` becomes ambiguous.
type Pro a = forall (n :: Nat). a n -> a ('S n)

-- Like `run`, except algebra is taken from typeclass instead of passed in.
-- However, still need to pass in promotion function since this is not
-- in a typeclass.
eval :: (OpAlg f a, ScopeAlg g a) => (r -> a 'Z) -> Pro a -> Prog f g r -> a 'Z
eval gen pro = run gen alg' where
    alg' = A alg dem pro

evalId :: (OpAlg f (CarrierId a), ScopeAlg g (CarrierId a)) => (r -> CarrierId a 'Z) -> Prog f g r -> a
evalId gen = runId gen alg' where
    alg' = A alg dem pro
    pro (Id x) = Id x

instance (OpAlg f a, OpAlg g a) => OpAlg (f :+: g) a where
    alg (L x) = alg x
    alg (R x) = alg x

instance (ScopeAlg f a, ScopeAlg g a) => ScopeAlg (f :+: g) a where
    dem (L x) = dem x
    dem (R x) = dem x

-- Handle rest of tree that isn't handled by algebra, i.e. if handling
-- Prog (Ask r :+: f) (LocalR r :+: g) a, this function handles f.
type HandleRestAlg h a = forall (n :: Nat) . h (a n) -> a n

-- Handle rest of tree that isn't handled by demotion, i.e. if handling
-- Prog (Ask r :+: f) (LocalR r :+: g) a, this function handles g.
type HandleRestDem i a = forall (n :: Nat) . i (a ('S n)) -> a n

-- Helper function for creating composite handlers which can be extended using
-- Data Types a la Carte.
evalHandler :: (Functor h, Functor i, OpAlg f a, ScopeAlg g a)
  => (r -> a 'Z)
  -> Pro a
  -> HandleRestAlg h a
  -> HandleRestDem i a
  -> Prog (f :+: h) (g :+: i) r
  -> a 'Z

evalHandler gen pro restAlg restSc prog = run gen (A a d pro) prog where
    a (L op) = alg op
    a (R op) = restAlg op

    d (L op) = dem op
    d (R op) = restSc op

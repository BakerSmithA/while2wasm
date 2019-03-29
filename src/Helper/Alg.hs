
-- Used to define algebras for use with Datatypes a la Carte methods.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE DataKinds, KindSignatures, RankNTypes #-}

module Helper.Alg
( module Helper.Prog
, OpAlg(..)
, ScopeAlg(..)
, Pro
, eval
, evalM
) where

import Helper.Prog
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

-- Like `runM`, except algebra is taken from typeclass.
evalM :: (Monad m, OpAlg f (CarrierM m a), ScopeAlg g (CarrierM m a))
      => Prog f g a -> m a
evalM = runM alg' where
    alg' = A alg dem pro
    pro (M x) = M (return (CSM x))

instance (OpAlg f a, OpAlg g a) => OpAlg (f :+: g) a where
    alg (L x) = alg x
    alg (R x) = alg x

instance (ScopeAlg f a, ScopeAlg g a) => ScopeAlg (f :+: g) a where
    dem (L x) = dem x
    dem (R x) = dem x

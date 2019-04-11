
-- Factoring out of Carrier and Carrier' from Syntax and Semantics paper.

{-# LANGUAGE KindSignatures, DataKinds, GADTs #-}

module Helper.Scope.Nest
( Nest(..)
, Nest'(..)
, Nest1(..)
, Nest1'(..)
) where

import Helper.Scope.Prog

-- TODO: Unify these.
data Nest f n = Nest { runNest :: f (Nest' f n) }

data Nest' f :: Nat -> * where
    NZ :: a -> Nest' f 'Z
    NS :: f (Nest' f n) -> Nest' f ('S n)

data Nest1 f a n = Nest1 { runNest1 :: f (Nest1' f a n) }

data Nest1' f a :: Nat -> * where
    NZ1 :: a -> Nest1' f a 'Z
    NS1 :: f (Nest1' f a n) -> Nest1' f a ('S n)

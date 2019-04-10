
-- Factoring out of Carrier and Carrier' from Syntax and Semantics paper.

{-# LANGUAGE KindSignatures, DataKinds, GADTs #-}

module Helper.Scope.Nest
( Nest(..)
, Nest'(..)
) where

import Helper.Scope.Prog

data Nest f a n = Nest { runNest :: f (Nest' f a n) }

data Nest' f a :: Nat -> * where
    NZ :: a -> Nest' f a 'Z
    NS :: f (Nest' f a n) -> Nest' f a ('S n)

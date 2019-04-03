
-- Base case to evaulate when using composite effect handlers.

{-# LANGUAGE DeriveFunctor, KindSignatures, DataKinds #-}

module Helper.Eff.Void
( Void
, handleVoid
) where

import Helper.Prog

-- Base case for composition.

data Void k deriving Functor

-- No recursion, therefore no Carrier'
data CarrierV a (n :: Nat) = V a

genV :: a -> CarrierV a 'Z
genV = V

-- Void does not have an constructors, therefore the algebras are undefined.
algV :: Alg Void Void (CarrierV a)
algV = A undefined undefined undefined

-- Essentially the same as Void handler in Fusion for Free (p.6).
-- The algebra is undefined for all nodes, and the base case is the identity -
-- it wraps up `x` in `V` then unwraps it.
handleVoid :: Prog Void Void a -> a
handleVoid prog = case run genV algV prog of
    (V x) -> x

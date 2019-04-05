
-- Generic reader effect handler, useful for constant 'global' variables.

{-# LANGUAGE DeriveFunctor, TypeOperators, GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff.Reader
( Ask
, ask
, handleReader
) where

import Helper.Prog
import Helper.Co
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Ask r k
    -- Retrieve the static environment.
    = Ask' (r -> k)
    deriving Functor

data LocalR r k
    -- Inside nested continuation, ask will return the value supplied to LocalR
    = LocalR' r k
    deriving Functor

pattern Ask fk <- (prj -> Just (Ask' fk))
ask :: (Functor f, Functor g, Ask r :<: f) => Prog f g r
ask = inject (Ask' Var)

pattern LocalR r k <- (prj -> Just (LocalR' r k))
localR :: (Functor f, Functor g, LocalR r :<: g) => r -> Prog f g a -> Prog f g a
localR r inner = injectS (fmap (fmap return) (LocalR' r inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- No scoping, therefore can use CarrierId.
type Carrier f g r a = CarrierId (r -> Prog f g a)

genR :: (Functor f, Functor g) => a -> Carrier f g r a 'Z
genR x = Id $ \_ -> return x

algR :: (Functor f, Functor g) => Alg (Ask r :+: f) g (Carrier f g r a)
algR = A a d p where
    a :: (Functor f, Functor g) => (Ask r :+: f) (Carrier f g r a n) -> Carrier f g r a n
    a (Ask fk)   = Id $ \r -> unId (fk r) r
    a (Other op) = Id $ \r -> Op (fmap (\(Id p) -> p r) op)

    d :: (Functor f, Functor g) => g (Carrier f g r a ('S n)) -> Carrier f g r a n
    d op = Id $ \r -> (Scope (fmap (\(Id p) -> return (p r)) op))

    p :: (Functor f, Functor g) => Carrier f g r a n -> Carrier f g r a ('S n)
    p (Id p) = Id p

handleReader :: (Functor f, Functor g) => r -> Prog (Ask r :+: f) g a -> Prog f g a
handleReader r prog = (runId genR algR prog) r

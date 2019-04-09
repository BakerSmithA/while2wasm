
-- Generic reader effect handler, useful for constant 'global' variables.

{-# LANGUAGE DeriveFunctor, TypeOperators, GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff.Reader
( Ask
, LocalR
, ask
, localR
, handleReader
) where

import Helper.Scope.Prog
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
ask = injectP (Ask' Var)

pattern LocalR r k <- (prj -> Just (LocalR' r k))
localR :: (Functor f, Functor g, LocalR r :<: g) => r -> Prog f g a -> Prog f g a
localR r inner = injectPSc (fmap (fmap return) (LocalR' r inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier f g r a n
    = Re { runR :: r -> Prog f g (Carrier' f g r a n) }

data Carrier' f g r a :: Nat -> * where
    CZ :: a -> Carrier' f g r a 'Z
    CS :: (r -> Prog f g (Carrier' f g r a n)) -> Carrier' f g r a ('S n)

genR :: (Functor f, Functor g) => a -> Carrier f g r a 'Z
genR x = Re (const (return (CZ x)))

algR :: (Functor f, Functor g) => Alg (Ask r :+: f) (LocalR r :+: g) (Carrier f g r a)
algR = A a d p where
    a :: (Functor f, Functor g) => (Ask s :+: f) (Carrier f g s a n) -> Carrier f g s a n
    a (Ask fk)   = Re $ \r -> runR (fk r) r
    a (Other op) = Re $ \r -> Op (fmap (\(Re run) -> run r) op)

    d :: (Functor f, Functor g) => (LocalR s :+: g) (Carrier f g s a ('S n)) -> Carrier f g s a n
    d (LocalR r' k) = Re $ \r -> do
        -- Run inner with new environment.
        CS run' <- runR k r'
        -- Run remaining continuation with original environment.
        run' r

    d (Other op) = Re $ \r -> Scope (fmap (\(Re run) -> fmap (f r) (run r)) op) where
        f :: r -> Carrier' f g r a ('S n) -> Prog f g (Carrier' f g r a n)
        f r (CS run') = run' r

    p :: (Functor f, Functor g) => Carrier f g r a n -> Carrier f g r a ('S n)
    p (Re run) = Re $ \_ -> return (CS run)

handleReader :: (Functor f, Functor g) => r -> Prog (Ask r :+: f) (LocalR r :+: g) a -> Prog f g a
handleReader r prog = do
    CZ prog' <- runR (run genR algR prog) r
    return prog'

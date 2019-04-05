
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

data Ask s k
    -- Retrieve the static environment.
    = Ask' (s -> k)
    deriving Functor

pattern Ask fk <- (prj -> Just (Ask' fk))
ask :: (Functor f, Functor g, Ask s :<: f) => Prog f g s
ask = inject (Ask' Var)

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- No scoping, therefore can use CarrierId.
type Carrier f g s a = CarrierId (s -> Prog f g a)

genR :: (Functor f, Functor g) => a -> Carrier f g s a 'Z
genR x = Id $ \_ -> return x

algR :: (Functor f, Functor g) => Alg (Ask s :+: f) g (Carrier f g s a)
algR = A a d p where
    a :: (Functor f, Functor g) => (Ask s :+: f) (Carrier f g s a n) -> Carrier f g s a n
    a (Ask fk)   = Id $ \s -> unId (fk s) s
    a (Other op) = Id $ \s -> Op (fmap (\(Id p) -> p s) op)

    d :: (Functor f, Functor g) => g (Carrier f g s a ('S n)) -> Carrier f g s a n
    d op = Id $ \s -> (Scope (fmap (\(Id p) -> return (p s)) op))

    p :: (Functor f, Functor g) => Carrier f g s a n -> Carrier f g s a ('S n)
    p (Id p) = Id p

handleReader :: (Functor f, Functor g) => s -> Prog (Ask s :+: f) g a -> Prog f g a
handleReader s prog = (runId genR algR prog) s


-- Generalised compositional state effect handler.

{-# LANGUAGE DeriveFunctor, TypeOperators, GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff.State where

import Helper.Prog
import Helper.Co
import Helper.Eff
import Helper.Eff.Void

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data State s k
    -- Gives the current state to the continuation.
    = Get' (s -> k)
    -- Sets the current state.
    | Put' s k
    deriving Functor

data Local s k
    -- Creates a block where state is local, and restored outside.
    = Local' s k
    deriving Functor

pattern Get fk <- (prj -> Just (Get' fk))
get :: (Functor f, Functor g, State s :<: f) => Prog f g s
get = inject (Get' Var)

pattern Put s k <- (prj -> Just (Put' s k))
put :: (Functor f, Functor g, State s :<: f) => s -> Prog f g ()
put s = inject (Put' s (Var ()))

pattern Local s k <- (prj -> Just (Local' s k))
local :: (Functor f, Functor g, Local s :<: g) => s -> Prog f g a -> Prog f g a
local s inner = injectS (fmap (fmap return) (Local' s inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier f g s a n
    = St { runSt :: s -> Prog f g (Carrier' f g s a n, s) }

data Carrier' f g s a :: Nat -> * where
    CZ :: a -> Carrier' f g s a 'Z
    CS :: (s -> Prog f g (Carrier' f g s a n, s)) -> Carrier' f g s a ('S n)

genSt :: (Functor f, Functor g) => a -> Carrier f g s a 'Z
genSt x = St (\s -> return (CZ x, s))

algSt :: (Functor f, Functor g) => Alg (State s :+: f) (Local s :+: g) (Carrier f g s a)
algSt = A a d p where
    a :: (Functor f, Functor g) => (State s :+: f) (Carrier f g s a n) -> Carrier f g s a n
    a (Get fk)   = St $ \s -> runSt (fk s) s
    a (Put s' k) = St $ \_ -> runSt k s'
    a (Other op) = St $ \s -> Op (fmap (\(St run) -> run s) op)

    d :: (Functor f, Functor g) => (Local s :+: g) (Carrier f g s a ('S n)) -> Carrier f g s a n
    d = undefined

    p :: (Functor f, Functor g) => Carrier f g s a n -> Carrier f g s a ('S n)
    p = undefined

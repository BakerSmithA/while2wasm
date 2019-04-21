
-- Generic state effect handler.

{-# LANGUAGE DeriveFunctor, TypeOperators, GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff.State
( State
, LocalSt
, get
, put
, modify
, localSt
, handleState
) where

import Helper.Scope.Prog
import Helper.Co
import Helper.Inj
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

data LocalSt s k
    -- Creates a block where state is local. State after the local block is
    -- restored to state before entering.
    = LocalSt' s k
    deriving Functor

pattern Get fk <- (prj -> Just (Get' fk))
get :: (Functor f, Functor g, State s :<: f) => Prog f g s
get = injectP (Get' Var)

pattern Put s k <- (prj -> Just (Put' s k))
put :: (Functor f, Functor g, State s :<: f) => s -> Prog f g ()
put s = injectP (Put' s (Var ()))

modify :: (Functor f, Functor g, State s :<: f) => (s -> s) -> Prog f g ()
modify f = do
    env <- get
    put (f env)

pattern LocalSt s k <- (prj -> Just (LocalSt' s k))
localSt :: (Functor f, Functor g, LocalSt s :<: g) => s -> Prog f g a -> Prog f g a
localSt s inner = injectPSc (fmap (fmap return) (LocalSt' s inner))

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

algSt :: (Functor f, Functor g) => Alg (State s :+: f) (LocalSt s :+: g) (Carrier f g s a)
algSt = A a d p where
    a :: (Functor f, Functor g) => (State s :+: f) (Carrier f g s a n) -> Carrier f g s a n
    a (Get fk)   = St $ \s -> runSt (fk s) s
    a (Put s' k) = St $ \_ -> runSt k s'
    a (Other op) = St $ \s -> Op (fmap (\(St run) -> run s) op)

    d :: (Functor f, Functor g) => (LocalSt s :+: g) (Carrier f g s a ('S n)) -> Carrier f g s a n
    d (LocalSt s' k) = St $ \s -> do
        -- Run nested continuation with inner-state.
        (CS run', s'') <- runSt k s'
        -- Run remaining continuation (after nested continuation) in original state.
        run' s

    d (Other op) = St $ \s -> Scope (fmap (\(St run) -> fmap f (run s)) op) where
        f :: (Carrier' f g s a ('S n), s) -> Prog f g (Carrier' f g s a n, s)
        f (CS run, s) = run s

    p :: (Functor f, Functor g) => Carrier f g s a n -> Carrier f g s a ('S n)
    p (St run) = St $ \s -> return (CS run, s)

handleState :: (Functor f, Functor g) => s -> Prog (State s :+: f) (LocalSt s :+: g) a -> Prog f g (a, s)
handleState s prog = do
    (CZ prog', s') <- runSt (run genSt algSt prog) s
    return (prog', s')

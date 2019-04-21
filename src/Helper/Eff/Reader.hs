
-- Generic reader effect handler, useful for constant 'global' variables.

{-# LANGUAGE DeriveFunctor, TypeOperators, GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, Rank2Types #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff.Reader
( Ask
, LocalR
, ask
, localR
, handleReader
) where

import Helper.Scope.Prog
import Helper.Scope.Alg
import Helper.Co
import Helper.Inj
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Ask r k
    -- Retrieve the static environment.
    = Ask (r -> k)
    deriving Functor

data LocalR r k
    -- Inside nested continuation, ask will return the value supplied to LocalR
    = LocalR r k
    deriving Functor

ask :: (Functor f, Functor g, Ask r :<: f) => Prog f g r
ask = injectP (Ask Var)

localR :: (Functor f, Functor g, LocalR r :<: g) => r -> Prog f g a -> Prog f g a
localR r inner = injectPSc (fmap (fmap return) (LocalR r inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier f g r a n
    = Re { runR :: r -> Prog f g (Carrier' f g r a n) }

data Carrier' f g r a :: Nat -> * where
    CZ :: a -> Carrier' f g r a 'Z
    CS :: (r -> Prog f g (Carrier' f g r a n)) -> Carrier' f g r a ('S n)

instance OpAlg (Ask r) (Carrier f g r a) where
    alg (Ask fk) = Re $ \r -> runR (fk r) r

instance (Functor f, Functor g) => ScopeAlg (LocalR r) (Carrier f g r a) where
    dem (LocalR r' k) = Re $ \r -> do
        -- Run inner with new environment.
        CS run' <- runR k r'
        -- Run remaining continuation with original environment.
        run' r

handleReader :: (Functor h, Functor i)
             => (OpAlg f (Carrier h i r a), ScopeAlg g (Carrier h i r a))
             => r -> Prog (f :+: h) (g :+: i) a -> Prog h i a

handleReader r prog = fmap (\(CZ prog') -> prog') (runR (evalHandler gen pro restAlg restDem prog) r) where
    gen :: (Functor h, Functor i) => a -> Carrier h i r a 'Z
    gen x = Re $ \_ -> return (CZ x)

    pro :: (Functor h, Functor i) => Carrier h i r a n -> Carrier h i r a ('S n)
    pro (Re run) = Re $ \_ -> return (CS run)

    restAlg :: Functor h => h (Carrier h i r a n) -> Carrier h i r a n
    restAlg op = Re $ \r -> Op (fmap (flip runR r) op)

    restDem :: (Functor h, Functor i) => i (Carrier h i r a ('S n)) -> Carrier h i r a n
    restDem op = Re $ \r -> Scope (fmap (\(Re run) -> fmap (f r) (run r)) op) where
        f :: r -> Carrier' f g r a ('S n) -> Prog f g (Carrier' f g r a n)
        f r (CS run') = run' r

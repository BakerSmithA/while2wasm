
-- Generic writer effect handler.

{-# LANGUAGE DeriveFunctor, TypeOperators, GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff.Writer
( Tell
, tell
, handleWriter
) where

import Helper.Scope.Prog
import Helper.Scope.Alg
import Helper.Scope.Nest
import Helper.Co
import Helper.Inj
import Helper.Eff
import Helper.Eff.Void

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Tell w k
    -- Output to environment.
    = Tell w k
    deriving Functor

tell :: (Functor f, Functor g, Tell w :<: f) => w -> Prog f g ()
tell w = injectP (Tell w (Var ()))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier f g w a n = W { runW :: Prog f g (Carrier' f g w a n, w) }

data Carrier' f g w a :: Nat -> * where
    CZ :: a -> Carrier' f g w a 'Z
    CS :: Prog f g (Carrier' f g w a n, w) -> Carrier' f g w a ('S n)

instance (Functor f, Functor g, Monoid w) => OpAlg (Tell w) (Carrier f g w a) where
    alg (Tell w (W k)) = W $ do
        (x, w') <- k
        return (x, w `mappend` w')

handleWriter' :: (Functor h, Functor i, Monoid w)
             => (OpAlg f (Carrier h i w a), ScopeAlg g (Carrier h i w a))
             => Prog (f :+: h) (g :+: i) a -> Carrier h i w a 'Z

handleWriter' prog = evalHandler gen pro restAlg restDem prog where
    gen :: (Functor h, Functor i, Monoid w) => a -> Carrier h i w a 'Z
    gen x = W (return (CZ x, mempty))

    pro :: (Functor h, Functor i) => Carrier h i w a n -> Carrier h i w a ('S n)
    pro (W prog) = W $ do
        (_, w) <- prog
        return (CS prog, w)

    restAlg :: (Functor h, Functor i) => h (Carrier h i w a n) -> Carrier h i w a n
    restAlg op = W (Op (fmap runW op))

    restDem :: (Functor h, Functor i) => i (Carrier h i w a ('S n)) -> Carrier h i w a n
    restDem op = W (Scope (fmap (\(W prog) -> fmap f prog) op)) where
        f :: (Carrier' h i w a ('S n), w) -> Prog h i (Carrier' h i w a n, w)
        f (CS prog, _) = prog

handleWriter :: (Functor h, Functor i, Monoid w)
             => (OpAlg f (Carrier h i w a), ScopeAlg g (Carrier h i w a))
             => Prog (f :+: h) (g :+: i) a -> Prog h i (a, w)
handleWriter prog = case handleWriter' prog of
    (W prog') -> do
        (CZ x, w) <- prog'
        return (x, w)


-- No scoping, therefore can use CarrierId.
-- type Carrier f g w a = CarrierId (Prog f g (a, w))
--
-- genW :: (Functor f, Functor g, Monoid w) => a -> Carrier f g w a 'Z
-- genW x = Id (return (x, mempty))
--
-- algW :: (Functor f, Functor g, Monoid w) => Alg (Tell w :+: f) g (Carrier f g w a)
-- algW = A a d p where
--     a :: (Functor f, Functor g, Monoid w) => (Tell w :+: f) (Carrier f g w a n) -> Carrier f g w a n
--     a (Tell w k) = Id $ do
--         (x, w') <- unId k
--         return (x, w `mappend` w')
--
--     a (Other op) = Id (Op (fmap unId op))
--
--     d :: (Functor f, Functor g) => g (Carrier f g w a ('S n)) -> Carrier f g w a n
--     d op = Id $ (Scope (fmap (return . unId) op))
--
--     p :: (Functor f, Functor g) => Carrier f g w a n -> Carrier f g w a ('S n)
--     p (Id p) = Id p
--
-- handleWriter :: (Functor f, Functor g, Monoid w) => Prog (Tell w :+: f) g a -> Prog f g (a, w)
-- handleWriter = runId genW algW

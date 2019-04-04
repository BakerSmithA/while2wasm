
-- Generic writer effect handler.

{-# LANGUAGE DeriveFunctor, TypeOperators, GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff.Writer
( Tell
, tell
, handleWriter
) where

import Helper.Prog
import Helper.Co
import Helper.Eff
import Helper.Eff.Void

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Tell w k
    -- Output to environment.
    = Tell' w k
    deriving Functor

pattern Tell w k <- (prj -> Just (Tell' w k))
tell :: (Functor f, Functor g, Tell w :<: f) => w -> Prog f g ()
tell w = inject (Tell' w (Var ()))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- No scoping, therefore can use CarrierId.
type Carrier f g w a = CarrierId (Prog f g (a, w))

genW :: (Functor f, Functor g, Monoid w) => a -> Carrier f g w a 'Z
genW x = Id (return (x, mempty))

algW :: (Functor f, Functor g, Monoid w) => Alg (Tell w :+: f) g (Carrier f g w a)
algW = A a d p where
    a :: (Functor f, Functor g, Monoid w) => (Tell w :+: f) (Carrier f g w a n) -> Carrier f g w a n
    a (Tell w k) = Id $ do
        (x, w') <- unId k
        return (x, w `mappend` w')

    a (Other op) = Id (Op (fmap unId op))

    d :: (Functor f, Functor g) => g (Carrier f g w a ('S n)) -> Carrier f g w a n
    d op = Id $ (Scope (fmap (return . unId) op))

    p :: (Functor f, Functor g) => Carrier f g w a n -> Carrier f g w a ('S n)
    p (Id p) = Id p

handleWriter :: (Functor f, Functor g, Monoid w) => Prog (Tell w :+: f) g a -> Prog f g (a, w)
handleWriter = runId genW algW

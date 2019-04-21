
-- Generic fresh effect handler, used to produce fresh Words starting a 0.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Helper.Eff.Fresh
( Fresh
, fresh
, handleFresh
) where

import Data.Word
import Helper.Scope.Prog
import Helper.Co
import Helper.Inj
import Helper.Scope.Alg
import Helper.Eff.State
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Fresh k
    -- Product a fresh value of type s.
    = Fresh' (Word -> k)
    deriving Functor

pattern Fresh fk <- (prj -> Just (Fresh' fk))
fresh :: (Functor f, Functor g, Fresh :<: f) => Prog f g Word
fresh = injectP (Fresh' Var)

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Expressed in terms of State effect handler (Effect Delegation from Fusion for Free).

type Carrier f g a = CarrierId (Prog (State Word :+: f) (LocalSt Word :+: g) a)

-- Use Datatypes a la Carte methods, i.e. typeclasses, to convert from Fresh
-- to State.

algF :: (Functor f, Functor g) => Alg (Fresh :+: f) g (Carrier f g a)
algF = A a d p where
    a :: (Functor f, Functor g) => (Fresh :+: f) (Carrier f g a n) -> Carrier f g a n
    a (Fresh fk) = Id $ do
        next <- get
        put (succ next)
        let (Id r) = fk next
        r
    a (Other op) = Id $ (Op (fmap unId (R op)))

    d :: (Functor f, Functor g) => g (Carrier f g a ('S n)) -> Carrier f g a n
    d op = Id (Scope (fmap (return . unId) (R op)))

    p :: (Functor f, Functor g) => Carrier f g a n -> Carrier f g a ('S n)
    p (Id prog) = Id prog

mkState :: (Functor f, Functor g) => Prog (Fresh :+: f) g a -> Prog (State Word :+: f) (LocalSt Word :+: g) a
mkState = runId (Id . return) algF

-- Takes initial fresh value, this will be returned the first time `fresh` is called.
handleFresh :: (Functor f, Functor g) => Word -> Prog (Fresh :+: f) g a -> Prog f g (a, Word)
handleFresh n = handleState n . mkState

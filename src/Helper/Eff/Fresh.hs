
-- Generic fresh effect handler, used to produce fresh Words starting a 0.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Helper.Eff.Fresh
( Fresh
, fresh
, handleFresh
) where

import Helper.Scope.Prog
import Helper.Co
import Helper.Inj
import Helper.Scope.Alg
import Helper.Eff.State
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Fresh e k
    -- Product a fresh value of type e.
    = Fresh' (e -> k)
    deriving Functor

pattern Fresh fk <- (prj -> Just (Fresh' fk))
fresh :: (Functor f, Functor g, Fresh e :<: f) => Prog f g e
fresh = injectP (Fresh' Var)

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Expressed in terms of State effect handler (Effect Delegation from Fusion for Free).

type Carrier f g e a = CarrierId (Prog (State e :+: f) (LocalSt e :+: g) a)

-- Use Datatypes a la Carte methods, i.e. typeclasses, to convert from Fresh
-- to State.

getE :: (Functor f, Functor g) => Prog (State e :+: f) (LocalSt e :+: g) e
getE = get

algF :: (Functor f, Functor g, Enum e) => Alg (Fresh e :+: f) g (Carrier f g e a)
algF = A a d p where
    a :: (Functor f, Functor g, Enum e) => (Fresh e :+: f) (Carrier f g e a n) -> Carrier f g e a n
    a (Fresh fk) = Id $ do
        next <- getE
        put (succ next)
        let (Id r) = fk next
        r
    a (Other op) = Id $ (Op (fmap unId (R op)))

    d :: (Functor f, Functor g) => g (Carrier f g e a ('S n)) -> Carrier f g e a n
    d op = Id (Scope (fmap (return . unId) (R op)))

    p :: (Functor f, Functor g) => Carrier f g e a n -> Carrier f g e a ('S n)
    p (Id prog) = Id prog

mkState :: (Functor f, Functor g, Enum e) => Prog (Fresh e :+: f) g a -> Prog (State e :+: f) (LocalSt e :+: g) a
mkState = runId (Id . return) algF

-- Takes initial fresh value, this will be returned the first time `fresh` is called.
handleFresh :: (Functor f, Functor g, Enum e) => e -> Prog (Fresh e:+: f) g a -> Prog f g (a, e)
handleFresh n = handleState n . mkState

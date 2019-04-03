
-- Generic fresh effect handler, used to produce fresh values of some type.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Helper.Eff.Fresh where

import Helper.Prog
import Helper.Co
import Helper.Alg
import Helper.Eff.State

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Fresh s k
    -- Product a fresh value of type s.
    = Fresh' (s -> k)
    deriving Functor

pattern Fresh fk <- (prj -> Just (Fresh' fk))
fresh :: (Functor f, Functor g, Fresh s :<: f) => Prog f g s
fresh = inject (Fresh' Var)

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Expressed in terms of State effect handler.

type Carrier f g s a = CarrierId (Prog (State s :+: f) (LocalSt s :+: g) a)

-- Use Datatypes a la Carte methods, i.e. typeclasses, to convert from Fresh
-- to State.

instance (Functor f, Functor g, Enum s) => OpAlg (Fresh s) (Carrier f g s a) where
    alg (Fresh' fk) = Id $ do
        next <- get
        put (succ next)
        let (Id r) = fk next
        r

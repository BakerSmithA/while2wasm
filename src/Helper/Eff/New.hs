
-- Generic fresh effect handler, used to produce new, fresh values.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Helper.Eff.New
( New
, new
, handleNew
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

data New e k
    -- Product a fresh value of type e.
    = New' (e -> k)
    deriving Functor

pattern New fk <- (prj -> Just (New' fk))
new :: (Functor f, Functor g, New e :<: f) => Prog f g e
new = injectP (New' Var)

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Expressed in terms of State effect handler (Effect Delegation from Fusion for Free).

type Carrier f g e a = CarrierId (Prog (State e :+: f) (LocalSt e :+: g) a)

-- Use Datatypes a la Carte methods, i.e. typeclasses, to convert from New
-- to State.

getE :: (Functor f, Functor g) => Prog (State e :+: f) (LocalSt e :+: g) e
getE = get

algF :: (Functor f, Functor g, Enum e) => Alg (New e :+: f) g (Carrier f g e a)
algF = A a d p where
    a :: (Functor f, Functor g, Enum e) => (New e :+: f) (Carrier f g e a n) -> Carrier f g e a n
    a (New fk) = Id $ do
        next <- getE
        put (succ next)
        let (Id r) = fk next
        r
    a (Other op) = Id $ (Op (fmap unId (R op)))

    d :: (Functor f, Functor g) => g (Carrier f g e a ('S n)) -> Carrier f g e a n
    d op = Id (Scope (fmap (return . unId) (R op)))

    p :: (Functor f, Functor g) => Carrier f g e a n -> Carrier f g e a ('S n)
    p (Id prog) = Id prog

mkState :: (Functor f, Functor g, Enum e) => Prog (New e :+: f) g a -> Prog (State e :+: f) (LocalSt e :+: g) a
mkState = runId (Id . return) algF

-- Takes initial fresh value, this will be returned the first time `new` is called.
handleNew :: (Functor f, Functor g, Enum e) => e -> Prog (New e:+: f) g a -> Prog f g (a, e)
handleNew n = handleState n . mkState

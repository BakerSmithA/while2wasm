
-- Exception effect handler and semantics, i.e. ability to 'throw' an error.
-- Therefore, the return type is wrapped in an Either.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module Helper.Eff.Exception where

import Helper.Prog
import Helper.Co
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- TODO
-- Throw an error of type e.
data Throw k
    = Throw' String k
    deriving Functor

-- Catch a thrown error of type e.
data Catch k
    = Catch' (String -> k)
    deriving Functor

pattern Throw err k <- (prj -> Just (Throw' err k))
throw :: Throw :<: f => String -> Prog f g ()
throw err = inject (Throw' err (Var ()))

pattern Catch hdl <- (prj -> Just (Catch' hdl))
catch :: (Functor f, Catch :<: g) => (String -> Prog f g ()) -> Prog f g ()
catch hdl = injectS (fmap (fmap return) (Catch' hdl))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Each time a catch is encountered, a new level of scope is entered and
-- a new handler is added to the top of 'stack'. This is the handler that will
-- be used if an exception is encountered inside the catch.
--
-- TODO
-- Therefore, the carrier we use is a stack of n error handlers, where Ntohing
-- is used to tie the recursive knot:
--  m (Either a e)

data CarrierExc f g a (n :: Nat)
    = Exc (Prog f g (Either String a))

data CarrierExc' f g a :: Nat -> * where
    CZ :: a -> CarrierExc' f g a 'Z
    CS :: [CarrierExc' f g a n] -> CarrierExc' f g a ('S n)

algExc :: (Functor f, Functor g) => Alg (Throw :+: f) (Catch :+: g) (CarrierExc f g a)
algExc = A a d p where
    a :: (Functor f, Functor g) => (Throw :+: f) (CarrierExc f g a n) -> CarrierExc f g a n
    a (Throw err k) = undefined
    a (Other op)    = undefined

    d :: (Functor f, Functor g) => (Catch :+: g) (CarrierExc f g a ('S n)) -> CarrierExc f g a n
    d (Catch hdl) = undefined
    d (Other op)  = undefined

    p :: CarrierExc f g a n -> CarrierExc f g a ('S n)
    p = undefined

-- Converts any program which might thrown an exception into a program that
-- returns either its result, or an error.
handleExc :: (Functor f, Functor g) => Prog (Throw :+: f) (Catch :+: g) a -> Prog f g (Either e a)
handleExc prog = undefined


-- Coproduct for use with Datatypes a la Carte and Extensible effects.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Helper.Co
( module Helper.Prog
, (:+:)(..)
, (:<:)(..)
, inject
, injectS
) where

import Helper.Prog

infixr 5 :+:

data (f :+: g) e
    = L (f e)
    | R (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (L x) = L (fmap f x)
    fmap f (R x) = R (fmap f x)

class (Functor sub, Functor sup) => sub :<: sup where
    -- Inject sub a into sup a
    inj :: sub a -> sup a
    -- Partial inverse, useful for pattern matching on expressions.
    prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
    inj = id
    prj = Just

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
    inj = L

    prj (L fa) = Just fa
    prj _      = Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = R . inj

    prj (R ga) = prj ga
    prj _      = Nothing

-- Inject into the non-scoped instruction of a Prog tree.
inject :: (f :<: h) => f (Prog h g a) -> Prog h g a
inject = Op . inj

-- Inject into the scoped instruction of a Prog tree.
injectS :: (h :<: g) => h (Prog f g (Prog f g a)) -> Prog f g a
injectS = Scope . inj

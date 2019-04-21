
-- Coproduct for use with Datatypes a la Carte and Extensible effects.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Helper.Co
( (:+:)(..)
) where

infixr 5 :+:

data (f :+: g) e
    = L (f e)
    | R (g e)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (L x) = L (fmap f x)
    fmap f (R x) = R (fmap f x)


-- Used to define algebras over Free for use with Datatypes a la Carte methods.

{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}

module Helper.Free.Alg where

import Helper.Co
import Helper.Free.Free

class Functor f => Alg f a where
    alg :: f a -> a

-- To define an algebra for the coproduct f :+: g, all we need to do is define
-- an a albegra for the individual functors.

instance (Alg f a, Alg g a) => Alg (f :+: g) a where
    alg (L x) = alg x
    alg (R x) = alg x

evalF :: Alg f b => (a -> b) -> Free f a -> b
evalF = foldFree alg

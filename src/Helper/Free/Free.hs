{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

module Helper.Free.Free
( Free(..)
, Freeable(..)
, foldFree
) where

data Free f a
    = Pure a
    | Free (f (Free f a))
    deriving Functor

instance Functor f => Applicative (Free f) where
    pure = Pure
    Pure f <*> a = fmap f a
    Free f <*> a = Free $ fmap (<*> a) f

instance (Functor f) => Monad (Free f) where
    return = Pure
    Pure a >>= k = k a
    Free m >>= k = Free ((>>= k) <$> m)

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
deriving instance (Eq a, Eq (f (Free f a)))     => Eq (Free f a)

foldFree :: Functor f => (f b -> b) -> (a -> b) -> Free f a -> b
foldFree _ base (Pure x)   = base x
foldFree f base (Free op) = f (fmap (foldFree f base) op)

-- For conversion from recursively defined types to Free types.
class Functor f => Freeable p f where
    free :: p -> Free f a

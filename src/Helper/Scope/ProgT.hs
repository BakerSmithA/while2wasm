
-- Inspired by FreeT
-- http://www.haskellforall.com/2012/07/free-monad-transformers.html

{-# LANGUAGE DeriveFunctor, KindSignatures, DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Helper.Scope.ProgT where

import Helper.Scope.Nat

-- FreeT defined as:

-- data FreeF f a x = Pure a | Free (f x)
-- newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }

data ProgF (f :: * -> *) (g :: * -> *) (x :: * -> *) (a :: *)
    = Var a
    | Op (f (x a))
    | Scope (g (x (x a)))
    deriving Functor

newtype ProgT (f :: * -> *) (g :: * -> *) (m :: * -> *) (a :: *) = ProgT {
    runProgT :: m (ProgF f g (ProgT f g m) a)
} deriving Functor

op :: Monad m => f (ProgT f g m a) -> ProgT f g m a
op = ProgT . return . Op

scope :: Monad m => g (ProgT f g m (ProgT f g m a)) -> ProgT f g m a
scope = ProgT . return . Scope

instance (Monad m, Functor f, Functor g) => Applicative (ProgT f g m) where
    pure = ProgT . return . Var

    m <*> x = ProgT $ do
        f <- runProgT m
        runProgT $ case f of
            Var   f -> fmap f x
            Op    f -> op (fmap (<*> x) f)
            Scope f -> scope (fmap (fmap (<*> x)) f)

instance (Monad m, Functor f, Functor g) => Monad (ProgT f g m) where
    return = pure

    m >>= f = ProgT $ do
        x <- runProgT m
        runProgT $ case x of
            Var   a -> f a
            Op    a -> op (fmap (>>= f) a)
            Scope a -> scope (fmap (fmap (>>= f)) a)

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

data AlgT f g m a = A {
    a :: forall n. f (m (a n)) -> m (a n)
  , d :: forall n. g (m (a (S n))) -> m (a n)
  , p :: forall n. m (a n) -> m (a (S n))
}

fold :: (Functor f, Functor g, Monad m) => AlgT f g m a -> ProgT f g m (m (a n)) -> m (a n)
fold alg prog = do
    x <- runProgT prog
    case x of
        Var   x -> x
        Op    x -> a alg (fmap (fold alg) x)
        Scope x -> d alg (fmap (fold alg . fmap (p alg . fold alg)) x)

run :: (Monad m, Functor f, Functor g) => (r -> m (a 'Z)) -> AlgT f g m a -> ProgT f g m r -> m (a 'Z)
run gen alg prog = fold alg (fmap gen prog)

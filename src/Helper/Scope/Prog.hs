
-- Implementation of Prog from:
--  https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/lics2018.pdf

{-# LANGUAGE DeriveFunctor, ExistentialQuantification #-}
{-# LANGUAGE DataKinds, Rank2Types, KindSignatures, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Helper.Scope.Prog
( Prog(..)
, Nat(..)
, Alg(..)
, Progable(..)
, CarrierId(..)
, fold
, run
, runId
) where

import Helper.Scope.Nat

data Prog f g a
    -- Leaf node
    = Var a
    -- Normal non-scoped instruction, in case of programming languages
    | Op (f (Prog f g a))
    -- Represents scope. Here `a` is `Prog f g a`, which says that the leaf
    -- must be another whole program!
    | Scope (g (Prog f g (Prog f g a)))
    deriving Functor

-- TODO: Show Scope
instance (Show a, Show (f (Prog f g a)), Show (g (Prog f g a))) => Show (Prog f g a) where
        show (Var x)    = "Var (" ++ show x ++ ")"
        show (Op op)    = "Op (" ++ show op ++ ")"
        show (Scope sc) = "Scope (TODO)"

instance (Functor f, Functor g) => Applicative (Prog f g) where
    pure  = Var

    Var   f <*> x = fmap f x
    Op    f <*> x = Op (fmap (<*> x) f)
    Scope f <*> x = Scope (fmap (fmap (<*> x)) f)

instance (Functor f, Functor g) => Monad (Prog f g) where
    return = Var

    Var x    >>= f = f x
    Op op    >>= f = Op (fmap (>>=f) op)
    Scope sc >>= f = Scope (fmap (fmap (>>=f)) sc)

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

data Alg f g a = A {
    -- Algebra for normal instructions, i.e. with Op
    a :: forall n. f (a n) -> a n
    -- Demotion: algebra for exiting scope, this is denoted by decrease in
    -- `n`. Here, g is required to tell you about where you are exiting from,
    -- e.g. If constructor for an if-statement.
  , d :: forall n. g (a ('S n)) -> a n
    -- Promotion: algebra for entering scope.
  , p :: forall n. a n -> a ('S n)
}

fold :: (Functor f, Functor g) => Alg f g a -> Prog f g (a n) -> a n
fold _   (Var x)    = x
fold alg (Op op)    = a alg (fmap (fold alg) op)
fold alg (Scope sc) = d alg (fmap (fold alg . fmap (p alg . fold alg)) sc)

-- For practical use, its convenient to use a function to turn a program's
-- unindexed return type `r` into the indexed carrier type `a Z` before
-- folding over the structure.

run :: (Functor f, Functor g) => (r -> a 'Z) -> Alg f g a -> Prog f g r -> a 'Z
run gen alg prog = fold alg (fmap gen prog)

data AlgM (f :: * -> *) (g :: * -> *) (m :: * -> *) (a :: Nat -> *) = AT {
    aT :: forall n. f (m (a n)) -> m (a n)
  , dT :: forall n. g (m (a ('S n))) -> m (a n)
  , pT :: forall n. m (a n) -> m (a ('S n))
}

foldM :: (Functor f, Functor g, Monad m) => AlgM f g m a -> Prog f g (m (a n)) -> m (a n)
foldM _   (Var x)    = x
foldM alg (Op op)    = aT alg (fmap (foldM alg) op)
foldM alg (Scope sc) = dT alg (fmap (foldM alg . fmap (pT alg . foldM alg)) sc)

runM :: (Functor f, Functor g, Monad m) => (r -> m (a 'Z)) -> AlgM f g m a -> Prog f g r -> m (a 'Z)
runM gen alg prog = foldM alg (fmap gen prog)
--------------------------------------------------------------------------------
-- Conversion from Recursive
--------------------------------------------------------------------------------

-- Like Freeable typeclass for Free trees (see LangEng slides), but allows
-- creation of Prog tree.

class Progable p f g where
    prog :: p -> Prog f g ()

--------------------------------------------------------------------------------
-- Convenience
--------------------------------------------------------------------------------

-- Carrier with no nesting.
data CarrierId a (n :: Nat) = Id { unId :: a }

runId :: (Functor f, Functor g) => (r -> CarrierId a 'Z) -> Alg f g (CarrierId a) -> Prog f g r -> a
runId gen alg prog = case run gen alg prog of
    (Id x) -> x


-- Implementation of Prog from:
--  https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/lics2018.pdf

{-# LANGUAGE DeriveFunctor, ExistentialQuantification #-}
{-# LANGUAGE DataKinds, Rank2Types, KindSignatures, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Helper.Prog
( Prog(..)
, Nat(..)
, Alg(..)
, Progable(..)
, CarrierM(..)
, CarrierM'(..)
, CarrierId(..)
, fold
, run
, runM
, runId
) where

data Prog f g a
    -- Leaf node
    = Var a
    -- Normal non-scoped instruction, in case of programming languages
    | Op (f (Prog f g a))
    -- Represents scope. Here `a` is `Prog f g a`, which says that the leaf
    -- must be another whole program!
    | Scope (g (Prog f g (Prog f g a)))
    deriving Functor


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

data Nat = Z | S Nat

data Alg f g a = A {
    -- Algebra for normal instructions, i.e. with Op
    a :: forall n. f (a n) -> a n
    -- Demotion: algebra for exiting scope, this is denoted by decrease in
    -- `n`. Here, g is required to tell you about where you are exiting from,
    -- e.g. If constructor for an if-statement.
  , d :: forall n. g (a (S n)) -> a n
    -- Promotion: algebra for entering scope.
  , p :: forall n. a n -> a (S n)
}

fold :: (Functor f, Functor g) => Alg f g a -> Prog f g (a n) -> a n
fold alg (Var x)    = x
fold alg (Op op)    = a alg (fmap (fold alg) op)
fold alg (Scope sc) = d alg (fmap (fold alg . fmap (p alg . fold alg)) sc)

-- For practical use, its convenient to use a function to turn a program's
-- unindexed return type `r` into the indexed carrier type `a Z` before
-- folding over the structure.

run :: (Functor f, Functor g) => (r -> a Z) -> Alg f g a -> Prog f g r -> a Z
run gen alg prog = fold alg (fmap gen prog)

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

-- Covenience carrier for converting Prog trees to be wrapped in monad context.
-- `n` nested monads representing result at each level of scope.
data CarrierM m a n = M (m (CarrierM' m a n))

data CarrierM' m a :: Nat -> * where
    CZM :: a -> CarrierM' m a 'Z
    CSM :: m (CarrierM' m a n) -> CarrierM' m a ('S n)

genM :: (Monad m) => a -> CarrierM m a 'Z
genM x = M (return (CZM x))

runM :: (Monad m, Functor f, Functor g) => Alg f g (CarrierM m a) -> Prog f g a -> m a
runM alg prog = case run genM alg prog of
    (M prog') -> do
        (CZM x) <- prog'
        return x


-- Carrier with no nesting.
-- TODO: What exactly does this mean?
-- Use if do not care about nesting?
data CarrierId a (n :: Nat) = Id a

genId :: a -> CarrierId a 'Z
genId = Id

runId :: (Functor f, Functor g) => Alg f g (CarrierId a) -> Prog f g a -> a
runId alg prog = case run genId alg prog of
    (Id x) -> x

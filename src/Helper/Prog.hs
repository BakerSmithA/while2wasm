
-- Implementation of Prog from:
--  https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/lics2018.pdf

{-# LANGUAGE DeriveFunctor, ExistentialQuantification, DataKinds, Rank2Types #-}

module Helper.Prog where

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

-- Note, when folding using algebra, you fold from leaves upwards. Therefore,
-- `p` is called before `d`.
--
-- `Nat` is used to tell how deep you are in the scope (?)
--
-- `a` is the carrier (?)
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

run :: (Functor f, Functor g) => (r -> a Z) -> Alg f g a -> (Prog f g r -> a Z)
run gen alg prog = fold alg (fmap gen prog)

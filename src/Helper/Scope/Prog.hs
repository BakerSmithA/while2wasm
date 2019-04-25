
-- Implementation of Prog from:
--  https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/lics2018.pdf

{-# LANGUAGE DeriveFunctor, ExistentialQuantification #-}
{-# LANGUAGE DataKinds, Rank2Types, KindSignatures, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances, ViewPatterns, PatternSynonyms #-}

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

--------------------------------------------------------------------------------
-- Prog using Smart Views
--------------------------------------------------------------------------------

data Prog f g a
    = Var' a
    | Op' (f (Prog f g a))
    | Scope' (g (Prog f g (Prog f g a)))
    | forall x. (Prog f g x) :>>= (x -> Prog f g a)

deriving instance (Functor f, Functor g) => Functor (Prog f g)

instance (Functor f, Functor g) => Applicative (Prog f g) where
    pure  = Var'

    Var'   f   <*> x = fmap f x
    Op'    f   <*> x = Op' (fmap (<*> x) f)
    Scope' f   <*> x = Scope' (fmap (fmap (<*> x)) f)
    (p :>>= f) <*> x = p :>>= fmap (<*> x) f

instance (Functor f, Functor g) => Monad (Prog f g) where
    return = Var'
    (>>=) = (:>>=)

data ProgView f g a
    = VarV a
    | OpV (f (Prog f g a))
    | ScopeV (g (Prog f g (Prog f g a)))

pattern Var   a  <- (viewP -> VarV a)
pattern Op    op <- (viewP -> OpV op)
pattern Scope sc <- (viewP -> ScopeV sc)

viewP :: (Functor f, Functor g) => Prog f g a -> ProgView f g a
viewP (Var' x)            = VarV x
viewP (Op' op)            = OpV op
viewP (Scope' sc)         = ScopeV sc
viewP ((m :>>= f) :>>= g) = viewP (m :>>= \x -> f x :>>= g)
viewP (Var'   a  :>>= f)  = viewP (f a)
viewP (Op'    op :>>= f)  = OpV (fmap (:>>= f) op)
viewP (Scope' sc :>>= f)  = ScopeV (fmap (fmap (:>>= f)) sc)

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

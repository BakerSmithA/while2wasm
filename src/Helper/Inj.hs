
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}

module Helper.Inj
( (:<:)(..)
, injectF
, injectP
, injectPSc
, injectT
, injectTSc
) where

import Helper.Scope.Prog
import Helper.Scope.ProgT (ProgT)
import qualified Helper.Scope.ProgT as T
import Helper.Free.Free
import Helper.Co

class (Functor sub, Functor sup) => sub :<: sup where
    -- Inject sub a into sup a
    inj :: sub a -> sup a
    -- Partial inverse, useful for pattern matching on expressions.
    prj :: sup a -> Maybe (sub a)

-- Base case.
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

-- Inject into Free tree.
injectF :: (f :<: g) => f (Free g a) -> Free g a
injectF = Free . inj

-- Inject into the non-scoped instruction of a Prog tree.
injectP :: (f :<: h) => f (Prog h g a) -> Prog h g a
injectP = Op . inj

-- Inject into the scoped instruction of a Prog tree.
injectPSc :: (h :<: g) => h (Prog f g (Prog f g a)) -> Prog f g a
injectPSc = Scope . inj

injectT :: (Monad m, f :<: h) => f (ProgT h g m a) -> ProgT h g m a
injectT = T.op . inj

injectTSc :: (Monad m, h :<: g) => h (ProgT f g m (ProgT f g m a)) -> ProgT f g m a
injectTSc = T.scope . inj

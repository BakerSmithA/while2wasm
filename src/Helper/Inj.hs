
-- Injection described in the paper Composing and Decomposing Data Types
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.643.3533&rep=rep1&type=pdf

{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Helper.Inj
( (:<:)(..)
, injectF
, injectP
, injectPSc
) where

import Helper.Scope.Prog
import Helper.Free.Free
import Helper.Co

data Pos = Here | LPos Pos | RPos Pos

data Res = Found Pos | NotFound

type family Elem (e :: * -> *) (p :: * -> *) :: Res where
    Elem e e = Found Here
    Elem e (l :+: r ) = Choose (Elem e l) (Elem e r )
    Elem e p = NotFound

type family Choose (l :: Res) (r :: Res) :: Res where
    Choose (Found x) y = Found (LPos x)
    Choose x (Found y) = Found (RPos y)
    Choose x y = NotFound

class Subsume (res :: Res) f g where
    inj :: f a -> g a

instance Subsume (Found Here) f f where
    inj = id

-- See link below for converting from using Proxy to using AllowAmbiguousTypes
-- https://stackoverflow.com/questions/51102411/using-proxy-in-a-typeclass
instance (Subsume (Found p) f l) => Subsume (Found (LPos p)) f (l :+: r) where
    inj = L . inj @(Found p)

instance Subsume (Found p) f r => Subsume (Found (RPos p)) f (l :+: r) where
    inj = R . inj @(Found p)

type f :<: g = Subsume (Elem f g) f g

-- Inject into Free tree.
injectF :: forall f g a . f :<: g => f (Free g a) -> Free g a
injectF = Free . inj @(Elem f g)

-- Inject into the non-scoped instruction of a Prog tree.
injectP :: forall f g h a . f :<: h => f (Prog h g a) -> Prog h g a
injectP = Op . inj @(Elem f h)

-- Inject into the scoped instruction of a Prog tree.
injectPSc :: forall f g h a . h :<: g => h (Prog f g (Prog f g a)) -> Prog f g a
injectPSc = Scope . inj @(Elem h g)

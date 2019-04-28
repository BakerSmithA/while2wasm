
{-# LANGUAGE DeriveFunctor, TypeOperators, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module DirtyNew where

import Data.Set (Set)
import qualified Data.Set as Set
import Front.AST
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Co
import Helper.Inj

-- Annotate tree with variables used at different points.

type UsedVars v = Set v

data Ann v k
    = Ann (UsedVars v) k
    deriving Functor

newtype Carrier v a = C (a, UsedVars v)

instance Functor (Carrier v) where
    fmap = undefined

instance Applicative (Carrier v) where
    pure = undefined
    (<*>) = undefined

-- binop :: Ord a => (c -> c -> c) -> (c, Set a) -> (c, Set a) -> (c, Set a)
-- binop op x y = (rx `op` ry, Set.union cx cy) where
--     (rx, cx) = x
--     (ry, cy) = y

instance VarExp v :<: f => FreeAlg (VarExp v) (Carrier v (Free f a)) where
    alg (GetVar v) = C (getVar v, Set.singleton v)

instance (Ord v, AExp :<: f) => FreeAlg AExp (Carrier v (Free f a)) where
    alg (Num n)   = C (num n, Set.empty)
    alg (Add x y) = add <$> x <*> y
    alg (Sub x y) = sub <$> x <*> y
    alg (Mul x y) = mul <$> x <*> y
--
-- instance (Ord v, BExp :<: f) => FreeAlg BExp (Carrier f a v) where
--     -- alg (T)       = (true, Set.empty)
--     -- alg (F)       = (false, Set.empty)
--     -- alg (Equ x y) = binop equ x y
--     alg (LEq x y) = leq  `tmap` x `cat` y
--     alg (And x y) = andB `tmap` x `cat` y
--     alg (Not x)   = notB `tmap` x
--
instance Stm :<: f => FreeAlg Stm (Carrier v (Free f a)) where
    -- alg (Skip)            = return skip
    -- alg (Export x)        = export <$> x
    alg (If cond t e)     = ifElse <$> cond <*> t <*> e
    -- alg (While cond body) = while  <$> cond <*> body
    -- alg (Comp s1 s2)      = comp   <$> s1   <*> s2

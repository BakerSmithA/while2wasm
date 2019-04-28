
-- Annotate procedures with all the variables used in their bodies.

{-# LANGUAGE DeriveFunctor, TypeOperators, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Transform.Capture.Annotate
( UsedVars
, Ann(..)
, annotateAST
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Front.AST
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Co
import Helper.Inj

type UsedVars v = Set v

data Ann v k
    = Ann (UsedVars v) k
    deriving Functor

annotate :: Ann v :<: f => UsedVars v -> Free f a -> Free f a
annotate used inner = injectF (Ann used inner)

newtype Carrier' v a = C { unC :: (a, UsedVars v) }

instance Functor (Carrier' v) where
    fmap f (C (x, us)) = C (f x, us)

instance Ord v => Applicative (Carrier' v) where
    pure x = C (x, Set.empty)
    C (f, us) <*> C (y, vs) = C (f y, Set.union us vs)

type Carrier f v a = Carrier' v (Free (Ann v :+: f) a)

instance (Ord v, VarExp v :<: f) => FreeAlg (VarExp v) (Carrier f v a) where
    alg (GetVar v) = C (getVar v, Set.singleton v)

instance (Ord v, AExp :<: f) => FreeAlg AExp (Carrier f v a) where
    alg (Num n)   = pure (num n)
    alg (Add x y) = add <$> x <*> y
    alg (Sub x y) = sub <$> x <*> y
    alg (Mul x y) = mul <$> x <*> y

instance (Ord v, BExp :<: f) => FreeAlg BExp (Carrier f v a) where
    alg (T)       = pure true
    alg (F)       = pure false
    alg (Equ x y) = equ  <$> x <*> y
    alg (LEq x y) = leq  <$> x <*> y
    alg (And x y) = andB <$> x <*> y
    alg (Not x)   = notB <$> x

instance (Ord v, VarStm v :<: f) => FreeAlg (VarStm v) (Carrier f v a) where
    alg (SetVar v (C (x, used))) = C (setVar v x, Set.insert v used)

instance (Ord v, Stm :<: f) => FreeAlg Stm (Carrier f v a) where
    alg (Skip)            = pure skip
    alg (Export x)        = export <$> x
    alg (If cond t e)     = ifElse <$> cond <*> t <*> e
    alg (While cond body) = while  <$> cond <*> body
    alg (Comp s1 s2)      = comp   <$> s1   <*> s2

instance (Ord v, BlockStm v p :<: f) => FreeAlg (BlockStm v p) (Carrier f v a) where
    alg (Block varDecls procDecls (C (body, bodyUsed)))
        = C (block vs ps body, used) where
            used         = Set.union vsUsed bodyUsed
            (vs, vsUsed) = annotateVars varDecls
            ps           = annotateProcs procDecls

annotateVars :: Ord v => [(v, Carrier f v a)] -> ([(v, Free (Ann v :+: f) a)], UsedVars v)
annotateVars = foldr f ([], Set.empty) where
    f (v, (C (x, xUsed))) (vs, used) = ((v, x):vs, Set.insert v (Set.union xUsed used))

-- Adds annotation wrapping body of procedure containing all the variables used
-- inside the body of the procedure.
annotateProcs :: Functor f => [(p, Carrier f v a)] -> [(p, Free (Ann v :+: f) a)]
annotateProcs = foldr f [] where
    f (p, (C (body, used))) ps = (p, annotate used body) : ps

annotateAST :: (Ord v, Functor f, FreeAlg f (Carrier f v a)) => Free f a -> (Free (Ann v :+: f) a, UsedVars v)
annotateAST prog = unC (evalF (pure . return) prog)

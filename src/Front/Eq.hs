
-- Equivalence for AST types, useful for unit testing.

module Front.Eq where

import Data.Functor.Classes (Eq1, liftEq)
import Front.AST

binEq :: (a -> b -> Bool) -> a -> a -> b -> b -> Bool
binEq eq a b c d = a `eq` c && b `eq` d

instance Eq v => Eq1 (VarExp v) where
    liftEq _ (GetVar x) (GetVar y) = x == y

instance Eq1 AExp where
    liftEq _  (Num x)   (Num y)   = x == y
    liftEq eq (Add a b) (Add c d) = binEq eq a b c d
    liftEq eq (Sub a b) (Sub c d) = binEq eq a b c d
    liftEq eq (Mul a b) (Mul c d) = binEq eq a b c d
    liftEq _  _         _         = False

instance Eq1 BExp where
    liftEq _  (T)       (T)       = True
    liftEq _  (F)       (F)       = True
    liftEq eq (Equ a b) (Equ c d) = binEq eq a b c d
    liftEq eq (LEq a b) (LEq c d) = binEq eq a b c d
    liftEq eq (And a b) (And c d) = binEq eq a b c d
    liftEq eq (Not x)   (Not y)   = x `eq` y
    liftEq _  _         _         = False

instance Eq v => Eq1 (VarStm v) where
    liftEq eq (SetVar v1 x1 k1) (SetVar v2 x2 k2)
        = v1 == v2 && x1 `eq` x2 && k1 `eq` k2

instance Eq p => Eq1 (ProcStm p) where
    liftEq eq (Call p1 k1) (Call p2 k2)
        = p1 == p2 && k1 `eq` k2

instance Eq1 Stm where
    liftEq eq (Skip k1)     (Skip k2)     = k1 `eq` k2
    liftEq eq (Export x k1) (Export y k2) = x `eq` y && k1 `eq` k2
    liftEq _  _             _             = False

instance Eq1 ScopeStm where
    liftEq eq (If b1 t1 e1) (If b2 t2 e2)
        = b1 `eq` b2 && t1 `eq` t2 && e1 `eq` e2
    liftEq eq (While b1 s1) (While b2 s2)
        = b1 `eq` b2 && s1 `eq` s2
    liftEq _ _ _ = False

instance (Eq v, Eq p) => Eq1 (BlockStm v p) where
    liftEq eq (Block vs1 ps1 s1) (Block vs2 ps2 s2) =
        let eqVs = and $ zipWith (\(v1, x1) (v2, x2) -> v1 == v2 && x1 `eq` x2) vs1 vs2
            eqPs = and $ zipWith (\(p1, b1) (p2, b2) -> p1 == p2 && b1 `eq` b2) ps1 ps2
            eqB  = s1 `eq` s2
        in eqVs && eqPs && eqB

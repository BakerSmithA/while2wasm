
-- Maps from AST to effect handler to produce map of variable names to
-- whether they should be stored as a value or pointer.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Transform.Capture.AST where

import Front.AST
import Transform.Capture.CaptureEff
import Helper.Alg

type Carrier v = CarrierId (Capture v)

instance Ord v => OpAlg (VarExp v) (Carrier v) where
    alg (GetVar v) = Id $ used v

instance OpAlg AExp (Carrier v) where
    alg (Num n)             = Id (return ())
    alg (Add (Id x) (Id y)) = Id $ do x; y
    alg (Sub (Id x) (Id y)) = Id $ do x; y
    alg (Mul (Id x) (Id y)) = Id $ do x; y

instance OpAlg BExp (Carrier v) where
    alg (T)                 = Id $ return ()
    alg (F)                 = Id $ return ()
    alg (Equ (Id x) (Id y)) = Id $ do x; y
    alg (LEq (Id x) (Id y)) = Id $ do x; y
    alg (And (Id x) (Id y)) = Id $ do x; y
    alg (Not (Id x))        = Id $ x

instance Ord v => OpAlg (VarStm v) (Carrier v) where
    alg = undefined
